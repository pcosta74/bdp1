# source('io.R')

# Cond. Prob:  P(A|B) = P(A&B)/P(B)
# Bayes Theor: P(B|A) = (P(B)*P(A|B))/P(A)
# NaiveBayes: argmax P(Ck) PRODi=1..n P(xi|Ck)

naivebayes<-function(formula, train.data = data.frame(), pred.data = NULL, percent.split=0.7) {
  
  t<-terms(formula, data=train.data, keep.order = TRUE)
  classvar<-rownames(attr(t,"factors"))[attr(t,"response")]

  list.attrs<-attr(t,"term.labels")
  
  if(!classvar %in% names(train.data))
    stop("Unknown class variable: ", classvar)

  if(all(list.attrs == classvar))
    stop("Empty attribute list")
  
  if(!is.null(pred.data) & !all(list.attrs %in% names(pred.data)))
    stop("Trainning and test dataframes do not match")

  list.attrs <-unique(c(classvar, list.attrs))
  attr(list.attrs,"classvar")<-classvar
  
  train.data<-nb.discretize(train.data)
  model<-nb.classifier(list.attrs, train.data)

  sd<-nb.split(train.data, percent.split)
  if(percent.split<1.0)
    test.model<-nb.classifier(list.attrs, sd$train)
  else
    test.model<-model
  test.data<-nb.predictor(test.model, list.attrs, sd$test, prob.cols=TRUE)
  attr(train.data,"test.mode")<-attr(sd$train,"test.mode")
  nb.print.train.info(model, train.data, test.data)
  rm(test.data)
  rm(sd)
  
  if(!is.null(pred.data)) {
    test.data<-nb.discretize(pred.data, attr(train.data,"discr.tbl"))
    time<-Sys.time()
    test.data<-nb.predictor(model, list.attrs, test.data)
    time<-capture.output(Sys.time()-time)
    nb.print.predict.info(test.data, names(model[[classvar]]), time)
    pred.data[[classvar]]<-test.data[[classvar]]
    return(pred.data)
  }
  
  return(TRUE)
}

#
nb.classifier<-function(list.attrs, data) {

  time<-Sys.time()
  classvar<-attr(list.attrs,"classvar")
  model<-sapply(list.attrs, nb.distr.table, classvar, data) 
  time<-capture.output(Sys.time()-time)
  
  if(length(model) == 0)
    stop("Unable to create model")
  
  attr(data,"classvar")<-classvar
  attr(model,"classvar")<-classvar
  attr(model,"train.time")<-sub("Time difference of ","",time)
  return(model)
}

#
nb.predictor<-function(model, list.attrs, data, prob.cols=FALSE) {
  
  classvar<-attr(list.attrs,"classvar")
    
  result<-apply(data, 1, function(row) {
    post.prob<-sapply(list.attrs, nb.cond.prob, classvar, model, row)
    
    # Additive approach (sum of logs)
    log.prob<-apply(post.prob, 1, function(r) sum(log(r)))
    label<-names(which.max(log.prob))

    if(prob.cols == TRUE) {
      # Classic definition
      std.prob<-apply(post.prob,1,prod)
      std.prob<-std.prob/sum(std.prob)
  
      result<-c(label,unlist(std.prob))
      names(result)<-c(nb.pred.colname(classvar),nb.prob.colname(names(std.prob)))
      return(result)
    }
    
    return(label)
  })
  
  if(prob.cols == TRUE)
    data[,rownames(result)]<-data.frame(t(result))
  else
    data[[classvar]]<-result
  
  attr(data,"classvar")<-classvar
  return(data)
}

nb.distr.table<-function(attr, classvar, data) {

  v.attrs<-unique(c(attr,classvar))
  tbl<-table(data[,v.attrs], dnn=v.attrs) + 1
  
  # for faster calculation later on
  if(length(v.attrs) == 2)
    tbl<-addmargins(tbl,1,FUN=list(list("[TOTAL]"=sum, "[ERROR]"=function(x) return(1))))
  
  return(tbl)
}

nb.cond.prob<-function(attr, classvar, model, row) {
  prob.tbl<-model[[attr]]
  
  if(attr == classvar) 
    return(prob.tbl)
  else {
    sum.row<-prob.tbl[nrow(prob.tbl)-1,]
    err.row<-prob.tbl[nrow(prob.tbl),]
    prob.tbl<-head(prob.tbl,-2)
    rslt<-tryCatch(
      { return(prob.tbl[row[[attr]],]/sum.row) },
      error=function(e) { return(err.row) }
    )
    return(rslt)
  }
}

nb.roc.auc<-function(xpt, pred.distr) {
  classnames<-levels(xpt)
  result<-sapply(classnames,function(c,df) {
      col<-nb.prob.colname(c)
      df<-df[order(df[,col],decreasing=TRUE),c("xpt",col)]
      cc<-df$xpt == c
      r2<-sum(which(!cc))
      n1<-sum(cc)
      n2<-sum(!cc)
      u2<-r2-(n2*(n2+1))/2
      vY<-cumsum(cc)/n1
      vX<-cumsum(!cc)/n2
      return(list(auc=u2/(n1*n2),vX=vX,vY=vY))
  }, data.frame(xpt,pred.distr), simplify=FALSE)
  return(result)
}

nb.discretize<-function(data, discr.tbl=NULL) {
  
  lst.cont<-!sapply(data,function(c) is.factor(c)|is.character(c))
  if(any(lst.cont)) {
    lst.cont<-names(which(lst.cont))
    message("Continuous attributes detected, applying discretization to: ",
            paste(lst.cont, collapse=", "))
    
    if(is.null(discr.tbl))
      discr.tbl<-apply(data[lst.cont], 2, quantile)
    attr(data, "discr.tbl")<-discr.tbl
    
    for(item in lst.cont) {
      if(all(levels(factor(data[[item]])) %in% c(0,1))) 
        data[[item]]<-factor(data[[item]] == TRUE)
      else
        data[[item]]<-factor(cut(data[[item]],unique(discr.tbl[,item]),
                                 include.lowest=TRUE))
    }
  }
  
  return(data)
}

nb.split<-function(data, percent.split=0.7) {

  train<-data
  test<-data
  
  attributes(train)<-attributes(data)
  attributes(test)<-attributes(data)
  
  if(is.numeric(percent.split)) {
    if(0 < percent.split & percent.split < 1) {
      n.rows<-round(nrow(data)*percent.split,digits=0)
      sample<-sort(sample(1:nrow(data), n.rows))

      train<-train[sample,]
      test<-test[-sample,]
      test.mode<-paste("split ",percent.split*100,"% train, remaider test")
#       name<-paste(attr(train,"relation"),"csv",sep=".")
#       write.data.frame(train,paste("train",name,sep="/"))
#       name<-paste(sub("train","test",attr(train,"relation")),"csv",sep=".")
#       write.data.frame(test,paste("test",name,sep="/"))
    }
    else if(percent.split == 1)
      test.mode<-"evaluate on training data"
    else
      stop("Invalid sampling percentage: ", percent.split)
  }

  attr(train,"test.mode")<-test.mode
  attr(test,"test.mode")<-test.mode
  
  return(list(train=train, test=test))
}

nb.print.train.info <- function(model, train.data, test.data) {

  classvar<-attr(model,"classvar")
  
  l<-levels(train.data[[classvar]])
  conf.matrix<-table(factor(test.data[[classvar]], levels=l),
                     factor(test.data[[nb.pred.colname(classvar)]], levels=l), 
                     dnn = list("value","prediction"))
  
  prd.distr<-test.data[,(ncol(train.data)+2):ncol(test.data)]
  prd.distr<-apply(prd.distr,2,as.numeric)
  
  xpt.distr<-matrix(0,nrow(prd.distr),ncol(prd.distr),dimnames=dimnames(prd.distr))
  xpt.distr<-t(sapply(seq(1:nrow(xpt.distr)), 
                function(i,r,m) { m[i,r[i]]<-1; return(m[i,]) },
                nb.prob.colname(test.data[[classvar]]), xpt.distr))
  
  roc.auc<-nb.roc.auc(test.data[[classvar]], prd.distr)

  nb.plot.roc(roc.auc)
  
  cat("=== Run information ===\n")
  nb.print.relation.info(train.data)
  
  cat("\n\n=== Classification model (full training set) ===\n\n")
  nb.print.model(model)
    
  mode<-ifelse(nrow(train.data)==nrow(test.data),"training set","split test")
  cat("\n\n=== Evaluation on",mode,"===")  
  cat("\n=== Summary ===\n")  
  nb.print.summary(conf.matrix, xpt.distr, prd.distr)
    
  cat("\n\n=== Detailed accuracy by class ===\n\n")
  nb.print.cm.accuracy(conf.matrix, roc.auc)
  
  cat("\n\n=== Confusion matrix===\n\n")
  print(conf.matrix)
  
  cat("\n\n")
}

nb.print.predict.info<-function(data, levels, time) {
  
  classvar<-attr(data,"classvar")
  
  cat("=== Prediction information ===\n\n")
  t<-table(factor(data[[classvar]],levels=levels))
  t<-round(rbind(prop.table(t), t), digits = 2)
  rownames(t)<-c("%","")
  print(t)
  cat("\nTime taken to evaluate dataset: ",
      sub("Time difference of ","",time))
  cat("\n\n")
}

nb.print.relation.info<-function(data) {
  n<-nchar(max(nrow(data),ncol(data)))
  r<-attr(data,"relation")
  o<-attr(data,"ommited")
  l<-c("relation:","instances:","attributes:","","test mode:")
  v<-c(ifelse(is.character(r),r,"unknown"), 
       format(nrow(data),width=n,trim=FALSE,justify="right"),
       format(ncol(data),width=n,trim=FALSE,justify="right"), 
       paste(names(data),collapse = ", "),
       attr(data,"test.mode"))
  
  f<-paste("%-", max(nchar(l)),"s %s\n",sep="",collapse="")
  for(k in seq_along(l))
    cat(sprintf(f,l[k],v[k]))
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
    c(paste(round(ratio,2)*100,"%",sep=""),paste(round(1-ratio,2)*100,"%",sep=""),rep("",2),"")
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
    c(TPR=tp/ifelse(sum(tp,fn),sum(tp,fn),1), #sensitivity
      FPR=fp/ifelse(sum(fp,tn),sum(fp,tn),1), #fall-out
      PPV=tp/ifelse(sum(tp,fp),sum(tp,fp),1), #precision
      REC=tp/ifelse(sum(tp,fn),sum(tp,fn),1), #recall
      TNR=tn/ifelse(sum(fp,tn),sum(fp,tn),1), #specificity
      #NVP=tn/sum(tn,fn),
      #FDR=fp/sum(fp,tp),
      #FNR=fn/sum(fn,tp), #miss-rate
      F1S=(2*tp)/ifelse(sum(2*tp,fp,fn),sum(2*tp,fp,fn),1),
      #ACC=sum(tp,tn)/sum(tp,fp,fn,fp),
      AUC=ra[[n]]$auc)
  }, cm, ra))
  wavg<-apply(apply(cm,1,sum)*mtx,2,sum)/ifelse(sum(cm),sum(cm),1)
  mtx<-round(rbind(mtx,wavg), digits = 3)
  colnames(mtx)<-c("TP Rate","FP Rate", "Precision", "Recall", "Specificity", "F1 Score", "ROC AUC")
  print(data.frame(mtx,row.names=c(rownames(cm),"Weighted Avg.")))  
}

nb.print.model<-function(model) {
  ndx<-which(names(model) == attr(model,"classvar"))
  
  lst<-sapply(model[-ndx],function(t) head(t,-1), simplify=FALSE)
  
  mtx<-Reduce(function(x,y) rbind(x,rep(NA,ncol(x)),y), lst)
  mtx<-rbind(round(model[[ndx]]/sum(model[[ndx]]),digits=2), 
             model[[ndx]],rep(NA,ncol(mtx)), mtx)

  lst<-sapply(names(lst), function(c,l) c(c,rep("",nrow(l[[c]]))), lst, 
              USE.NAMES=FALSE)
  names<-mapply(paste, c("%","", unlist(lst)), rownames(mtx),sep="  ")
  rownames(mtx)<-seq(1:nrow(mtx))
  
  df<-data.frame(names, mtx)
  names(df)<-c("CLASS",colnames(mtx))
  df<-as.data.frame(apply(df,2,function(x) ifelse(is.na(x),"",x)))
  print.data.frame(format(df, justify="left"), quote=FALSE, row.names=FALSE)
  
  df<-data.frame(
    c("Time taken to build model: ", "Space required to store model:"),
    values=c(attr(model,"train.time"), capture.output(object.size(model)))
  )
  names(df)<-c(" "," ")
  print.data.frame(format(df,justify="left"), quote=FALSE, row.names=FALSE)
}

nb.plot.roc<-function(ra) {
  plot(0, 0, type = "l", xlim=c(0,1), ylim=c(0,1),
       xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
  axis(1, seq(0.0,1.0,0.1))
  axis(2, seq(0.0,1.0,0.1))
  abline(h=seq(0.0,1.0,0.1), v=seq(0.0,1.0,0.1), col="gray", lty=3)
  abline(0,1, col="darkgray", lty=2)
  
  cl<-rainbow(length(ra))
  lg<-sapply(names(ra),function(n,l) sprintf("(%.3f) %s", round(l[[n]]$auc,3), n), ra)
  for(k in seq_along(ra)) {
    lines(ra[[k]]$vX,ra[[k]]$vY, col = cl[k], type = 'l')
  }
  
  legend('bottomright', legend=lg, lty=c(1,1), lwd=c(2.5,2.5), col=cl, title=expression(bold("AUC")), cex=0.85)
}

nb.prob.colname<-function(c) {
  return(paste("P.",c,sep=""))
}

nb.pred.colname<-function(c) {
  return(paste(c,"^",sep=""))
}

