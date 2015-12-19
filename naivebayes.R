# Cond. Prob:  P(A|B) = P(A&B)/P(B)
# Bayes Theor: P(B|A) = (P(B)*P(A|B))/P(A)
# NaiveBayes: argmax P(Ck) PRODi=1..n P(xi|Ck)

naivebayes<-function(formula, train.data = data.frame(), pred.data = NULL, percent.split=1) {
  
  t<-terms(formula, data=train.data, keep.order = TRUE)
  response<-rownames(attr(t,"factors"))[attr(t,"response")]

  list.attrs<-attr(t,"term.labels")
  
  if(!response %in% names(train.data))
    stop("Unknown class column: ", response)

  if(all(list.attrs == response))
    stop("Empty attribute list")
  
  if(!is.null(pred.data) & !all(list.attrs %in% names(pred.data)))
    stop("Trainning and test dataframes do not match")

  list.attrs <-unique(c(response, list.attrs))
  attr(list.attrs,"response")<-response
  
  train.data<-nb.discretize(train.data)
  test.data<-train.data
  if(is.numeric(percent.split)) {
    if(0 < percent.split & percent.split < 1) {
      n.rows<-round(nrow(train.data)*percent.split,digits=0)
      sample<-sort(sample(1:nrow(train.data), n.rows))

      test.data<-train.data[-sample,]
      train.data<-train.data[sample,]
      test.mode<-paste("split ",percent.split*100,"% train, remaider test")
    }
    else if(percent.split == 1)
      test.mode<-"evaluate on training data"
    else
      stop("Invalid sampling percentage: ", percent.split)
    
    attr(train.data,"test.mode")<-test.mode
  } 
  
  classifier<-nb.classifier(list.attrs, train.data)
  test.data<-nb.predictor(classifier, list.attrs, test.data, prob.cols=TRUE)
  nb.print.train.info(classifier, train.data, test.data)
  
  rm(test.data)

  if(!is.null(pred.data)) {
    pred.data<-nb.discretize(pred.data,attr(train.data,"discr.tbl"))
    pred.data<-nb.predictor(classifier, list.attrs, pred.data)
    nb.print.predict.info(pred.data, names(classifier[[response]]))
    return(pred.data)
  }
  
  return(TRUE)
}

#
nb.classifier<-function(list.attrs, data) {

  time<-Sys.time()
  response<-attr(list.attrs,"response")
  classifier<-sapply(list.attrs, nb.distr.table, response, data) 
  time<-capture.output(Sys.time()-time)
  
  if(length(classifier) == 0)
    stop("Unable to create classifier")
  
  attr(data,"response")<-response
  attr(classifier,"response")<-response
  attr(classifier,"train.time")<-sub("Time difference of ","",time)
  return(classifier)
}

#
nb.predictor<-function(classifier, list.attrs, data, prob.cols=FALSE) {
  
  response<-attr(list.attrs,"response")
    
  result<-apply(data, 1, function(row) {
    post.prob<-sapply(list.attrs, nb.cond.prob, response, classifier, row)
    
    # Classic definiion
    std.prob<-apply(post.prob,1,prod)
    std.prob<-std.prob/sum(std.prob)
    
    # Additive approach (sum of logs)
    log.prob<-apply(post.prob, 1, function(r) sum(log(r)))
    label<-names(which.max(log.prob))

    if(prob.cols == TRUE) {
      result<-c(label,unlist(std.prob))
      names(result)<-c(nb.pred.colname(response),nb.prob.colname(names(std.prob)))
      return(result)
    }
    
    return(factor(label))
  })
  
  if(prob.cols == TRUE)
    data[,rownames(result)]<-data.frame(t(result))
  else
    data[[response]]<-result
  
  attr(data,"response")<-response
  return(na.omit(data))
}

nb.distr.table<-function(attr, response, data) {

  v.attrs<-unique(c(attr,response))
  tbl<-table(data[,v.attrs], dnn=v.attrs) + 1
  
  # for faster calculation later on
  if(length(v.attrs) == 2)
    tbl<-addmargins(tbl,1,FUN=list(list("[TOTAL]"=sum)))
  
  return(tbl)
}

nb.cond.prob<-function(attr, response, classifier, row) {
  prob.tbl<-classifier[[attr]]
  
  if(attr == response) 
    return(prob.tbl)
  else {
    sum.row<-prob.tbl[nrow(prob.tbl),]
    prob.tbl<-head(prob.tbl,-1)
    rslt<-tryCatch(
      { return(prob.tbl[row[[attr]],]/sum.row) },
      error=function(e) { return(rep(1,ncol(prob.tbl))) }
    )
    return(rslt)
  }
}

nb.roc_auc<-function(xpt, pred.distr) {
  classnames<-levels(xpt)
  result<-sapply(classnames,function(c,df) {
      col<-nb.prob.colname(c)
      df<-df[order(df[,col],decreasing=TRUE),c("xpt",col)]
      cc<-df$xpt == c
      nc<-df$xpt != c
      r2<-sum(which(nc))
      n1<-nrow(df[cc,])
      n2<-nrow(df[nc,])
      u2<-r2-(n2*(n2+1))/2
      vY<-cumsum(cc)/sum(cc)
      vX<-cumsum(nc)/sum(nc)
      return(list(auc=u2/(n1*n2),vX=vX,vY=vY))
  }, data.frame(xpt,pred.distr), simplify=FALSE)
  return(result)
}

nb.print.predict.info<-function(data, levels) {

  response<-attr(data,"response")
  
  cat("=== Prediction information ===\n\n")
  t<-table(factor(data[[response]],levels=levels))
  t<-round(rbind(prop.table(t), t), digits = 2)
  rownames(t)<-c("%","")
  print(t)
  
  cat("\n\n")
}

nb.print.train.info <- function(classifier, train.data, test.data) {

  response<-attr(classifier,"response")
  
  l<-levels(train.data[[response]])
  conf.matrix<-table(factor(test.data[[response]], levels=l),
                     factor(test.data[[nb.pred.colname(response)]], levels=l), 
                     dnn = list("value","prediction"))
  
  prd.distr<-test.data[,(ncol(train.data)+2):ncol(test.data)]
  prd.distr<-apply(prd.distr,2,as.numeric)
  
  xpt.distr<-matrix(0,nrow(prd.distr),ncol(prd.distr),dimnames=dimnames(prd.distr))
  xpt.distr<-t(sapply(seq(1:nrow(xpt.distr)), 
                function(i,r,m) { m[i,r[i]]<-1; return(m[i,]) },
                nb.prob.colname(test.data[[response]]), xpt.distr))
  
  roc.auc<-nb.roc_auc(test.data[[response]], prd.distr)

  nb.plot_roc(roc.auc)
  
  cat("=== Run information ===\n")
  nb.print.relation.info(train.data)
  
  cat("\n\n=== Evaluation model ===\n\n")
  nb.print.classifier(classifier)
    
  cat("\n\n=== Summary ===\n")  
  nb.print.summary(conf.matrix, xpt.distr, prd.distr)
    
  cat("\n\n=== Detailed accuracy by class ===\n\n")
  nb.print.cm.accuracy(conf.matrix, roc.auc)
  
  cat("\n\n=== Confusion matrix===\n\n")
  print(conf.matrix)
  
  cat("\n\n")
}

nb.print.relation.info<-function(data) {
  n<-nchar(max(nrow(data),ncol(data)))
  r<-attr(data,"relation")
  df <- data.frame(
    c("relation:","instances:","attributes:","","test mode:"),
    c(ifelse(is.character(r),r,"unknown"), 
      format(nrow(data),width=n,trim=FALSE,justify="right"),
      format(ncol(data),width=n,trim=FALSE,justify="right"), 
      paste(names(data),collapse = ", "),
      attr(data,"test.mode")),
    check.rows = TRUE)
  colnames(df) <- c(" "," ")
  print(df, row.names = FALSE, right = FALSE)
}

nb.print.summary<-function(cm, xd, pd) {
  total<-sum(cm)
  mdiag<-sum(diag(cm))
  ratio<-mdiag/total
  
  s1<-sum(abs(pd-xd)/ncol(pd))
  s2<-sum((pd-xd)^2/ncol(pd))
      
  mae<-round(s1/nrow(pd), digits=4)
  rmse<-round(sqrt(s2/nrow(pd)), digits=4)
  
  df<-data.frame(
    c("Correctly classified:","Incorrectly classified:",
      "Mean absolute error:", "Root mean squared error:",
      "Number of instances:"),
    c(round(mdiag), total-mdiag, mae, rmse, total),
    c(paste(round(ratio,2),"%",sep=""),paste(round(1-ratio,2),"%",sep=""),rep("",2),"")
  )
  colnames(df)<-c(" "," ", " ")
  print(format(df,justify = "left"), row.names=FALSE)  
}

nb.print.cm.accuracy<-function(cm,ra) {
  mtx<-t(sapply(seq_along(diag(cm)),function(n,cm,ra) {
    tp<-cm[n,n]
    fn<-sum(cm[n,-n])
    fp<-sum(cm[-n,n])
    tn<-sum(cm[-n,-n])
    c(TPR=tp/sum(tp,fn), #sensitivity
      FPR=fp/sum(fp,tn), #fall-out
      PPV=tp/sum(tp,fp), #precision
      REC=tp/sum(tp,fn), #recall
      TNR=tn/sum(fp,tn), #specificity
      #NVP=tn/sum(tn,fn),
      #FDR=fp/sum(fp,tp),
      #FNR=fn/sum(fn,tp), #miss-rate
      F1S=(2*tp)/sum(2*tp,fp,fn),
      #ACC=sum(tp,tn)/sum(tp,fp,fn,fp),
      AUC=ra[[n]]$auc)
  }, cm, ra))
  wavg<-apply(apply(cm,1,sum)*mtx,2,sum)/sum(cm)
  mtx<-round(rbind(mtx,wavg), digits = 3)
  colnames(mtx)<-c("TP Rate","FP Rate", "Precision", "Recall", "Specificity", "F1 Score", "ROC AUC")
  print(data.frame(mtx,row.names=c(rownames(cm),"Weighted Avg.")))  
}

nb.print.classifier<-function(classifier) {
  ndx<-which(names(classifier) == attr(classifier,"response"))
  
  lst<-classifier[-ndx]
  
  mtx<-Reduce(function(x,y) rbind(x,rep(NA,ncol(x)),y), lst)
  mtx<-rbind(round(classifier[[ndx]]/sum(classifier[[ndx]]),digits=2), 
             classifier[[ndx]],rep(NA,ncol(mtx)), mtx)

  lst<-sapply(names(lst), function(c,l) c(c,rep("",nrow(l[[c]]))), lst, 
              USE.NAMES=FALSE)
  names<-mapply(paste, c("%","", unlist(lst)), rownames(mtx),sep="  ")
  rownames(mtx)<-seq(1:nrow(mtx))
  
  df<-data.frame(names, mtx)
  names(df)<-c("CLASS",colnames(mtx))
  df<-as.data.frame(apply(df,2,function(x) ifelse(is.na(x),"",x)))
  print.data.frame(format(df, justify="left"), quote=FALSE, row.names=FALSE)
  
  cat("\nTime taken to build model: ", attr(classifier,"train.time"))
}

nb.plot_roc<-function(ra) {
  plot(0, 0, type = "l", xlim=c(0,1), ylim=c(0,1),
       xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
  axis(1, seq(0.0,1.0,0.1))
  axis(2, seq(0.0,1.0,0.1))
  abline(h=seq(0.0,1.0,0.1), v=seq(0.0,1.0,0.1), col="gray", lty=3)
  abline(0,1, col="darkgray", lty=2)
  
  cl<-rainbow(length(ra))
  for(k in seq_along(ra)) {
    lines(ra[[k]]$vX,ra[[k]]$vY, col = cl[k], type = 'l')
  }
  legend(0.7, 0.3, names(ra), lty=c(1,1), lwd=c(2.5,2.5), col=cl, title = "AUC")
}

nb.prob.colname<-function(c) {
  return(paste("P.",c,sep=""))
}

nb.pred.colname<-function(c) {
  return(paste(c,"^",sep=""))
}

nb.discretize<-function(data, discr.tbl=NULL) {

  lst.cont<-!sapply(data,is.factor)
  if(any(lst.cont)) {
    lst.cont<-names(which(lst.cont))
    message("Continuous attributes detected, applying discretization to: ",
            paste(lst.cont, collapse=", "))

    if(is.null(discr.tbl))
      discr.tbl<-apply(data[lst.cont], 2, quantile)
    attr(data, "discr.tbl")<-discr.tbl

    for(item in lst.cont) {
      if(all(levels(factor(data[[item]])) %in% c(0,1))) 
        data[[item]]<-factor(s == TRUE)
      else
        data[[item]]<-factor(cut(data[[item]],unique(discr.tbl[,item]),
                                 include.lowest=TRUE))
    }
  }
  
  return(data)
}
