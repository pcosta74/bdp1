# Cond. Prob:  P(A|B) = P(A&B)/P(B)
# Bayes Theor: P(B|A) = (P(B)*P(A|B))/P(A)
# NaiveBayes: argmax P(Ck) PRODi=1..n P(xi|Ck)

naivebayes<-function(formula, train.data = data.frame(), test.data = data.frame(), percent.split=0.7) {
  
  t<-terms(formula, data=train.data, keep.order = TRUE)
  response<-rownames(attr(t,"factors"))[attr(t,"response")]

  list.attrs<-attr(t,"term.labels")
  
  if(!response %in% names(train.data))
    stop("Unknown class column: ", response)
  
  if(!all(list.attrs %in% names(test.data)))
    stop("Trainning and test dataframes do not match")
    
  sample.data<-train.data
  if(is.numeric(percent.split)) {
    if(0 < percent.split & percent.split < 1) {
      n.rows<-round(nrow(train.data)*(1-percent.split),digits=0)
      sample<-sort(sample(1:nrow(train.data), n.rows))

      sample.data<-train.data[sample,]
      train.data<-train.data[-sample,]
    }
    else if(percent.split != 1)
      stop("Invalid sampling percentage: ", percent.split)
  } 
  
  list.attrs <-unique(c(response, list.attrs))
  attr(list.attrs,"response")<-response

  if(all(list.attrs == response))
    stop("Empty attribute list")
  
  classifier<-nb.classifier(list.attrs, train.data)
  pred.data<-nb.predictor(classifier, list.attrs, sample.data)
  
  conf.matrix<-table(sample.data[[response]], pred.data[[response]], 
                     dnn = list("value","prediction"))
  
  nb.print.train.info(train.data, conf.matrix, classifier)
  
  pred.data<-nb.predictor(classifier, list.attrs, test.data)
  nb.print.predict.info(pred.data, response)
  
  return(pred.data)
}

#
nb.classifier<-function(list.attrs, data) {

  response<-attr(list.attrs,"response")
  classifier<-sapply(list.attrs, nb.distr.table, response, data) 

  if(length(classifier) == 0)
    stop("Unable to create classifier")
  
  attr(classifier,"response")<-response
  return(classifier)
}

#
nb.predictor<-function(classifier, list.attrs, data) {
  
  response<-attr(list.attrs,"response")
    
  data[[response]]<-apply(data, 1, function(row) {
    post.prob<-sapply(list.attrs, nb.cond.prob, response, classifier, row)
    
    # Additive approach (sum of logs)
    log.prob<-apply(post.prob, 1, function(r) sum(log(r)))
    label<-names(which.max(log.prob))

    return(label)
  }) 
  
  return(data)
}

nb.distr.table<-function(attr, response, data) {

  v.attrs<-unique(c(attr,response))
  tbl<-table(data[,v.attrs], dnn=v.attrs) + 1
  
  # for faster calculation later on
  if(length(v.attrs) == 2)
    tbl<-addmargins(tbl,1,FUN=list(list("[TOTAL]"=sum,"[UNKNOWN]"=function(x) return(1))))
  
  return(tbl)
}

nb.cond.prob<-function(attr, response, classifier, row) {
  prob.tbl<-classifier[[attr]]
  
  if(attr == response) 
    return(prob.tbl)
  else {
    sum.row<-prob.tbl[nrow(prob.tbl)-1,]
    unk.row<-prob.tbl[nrow(prob.tbl),]
    prob.tbl<-head(prob.tbl,-2)
    rslt<-tryCatch(
      { return(prob.tbl[row[[attr]],]/sum.row) },
      error=function(e) { return(unk.row) }
    )
    return(rslt)
  }
}

nb.print.predict.info<-function(pred.df, response) {

  cat("=== Prediction information ===\n\n")
  t<-table(pred.df[[response]])
  t<-rbind(t, prop.table(t))
  t<-round(t, digits = 5)
  rownames(t)<-c("Frequency","Percent")
  print(t)
  
  cat("\n\n")
}

nb.print.train.info <- function(data, conf.matrix, classifier) {
  
  cat("=== Run information ===\n")
  nb.print.relation.info(data)
  
  cat("\n\n=== Evaluation model ===\n\n")
  nb.print.classifier(classifier)
    
  cat("\n\n=== Summary ===\n")  
  nb.print.cm.summary(conf.matrix)
    
  cat("\n\n=== Detailed accuracy by class ===\n\n")
  nb.print.cm.accuracy(conf.matrix)
  
  cat("\n\n=== Confusion matrix===\n\n")
  print(conf.matrix)
  
  cat("\n\n")
}

nb.print.relation.info<-function(data) {
  df <- data.frame(
    c("relation:","instances:","attributes:",""),
    c(attr(data,"relation"), nrow(data), ncol(data), paste(names(data),collapse = ", ")),
    check.rows = TRUE)
  colnames(df) <- c(" "," ")
  print(df, row.names = FALSE, right = FALSE)
}

nb.print.cm.summary<-function(cm) {
  total<-sum(cm)
  mdiag<-sum(diag(cm))
  df<-data.frame(
    c("Correctly classified:","Incorrectly classified:","Accuracy rate:","Error rate:","Number of instances:"),
    c(mdiag,total-mdiag, signif(mdiag/total,5), signif(1-(mdiag/total),5),total)
  )
  colnames(df)<-c(" "," ")
  print(format(df,justify = "left"), row.names=FALSE)  
}

nb.print.cm.accuracy<-function(cm) {
  mtx<-t(sapply(seq_along(diag(cm)),function(n,cm) {
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
      ACC=sum(tp,tn)/sum(tp,fp,fn,fp))
  }, cm))
  wavg<-apply(apply(cm,1,sum)*mtx,2,sum)/sum(cm)
  mtx<-signif(rbind(mtx,c(wavg)), digits = 5)
  colnames(mtx)<-c("TP Rate","FP Rate", "Precision", "Recall", "Specificity", "F1 Score", "Accuracy")
  print(data.frame(mtx,row.names=c(rownames(cm),"Weighted Avg.")))  
}

nb.print.classifier<-function(classifier) {
  ndx<-which(names(classifier) == attr(classifier,"response"))
  
  lst<-sapply(classifier[-ndx], function(t) return(head(t, -1)))
  
  mtx<-Reduce(function(x,y) rbind(x,rep(NA,ncol(x)),y), lst)
  mtx<-rbind(classifier[[ndx]], rep(NA,ncol(mtx)), mtx)

  lst<-sapply(names(lst), function(c,l) c(c,rep("",nrow(l[[c]]))), lst, 
              USE.NAMES=FALSE)
  names<-mapply(paste, c("", unlist(lst)), rownames(mtx),sep="  ")
  rownames(mtx)<-seq(1:nrow(mtx))
  
  df<-data.frame(names, mtx)
  names(df)<-c("CLASS",colnames(mtx))
  df<-as.data.frame(apply(df,2,function(x) ifelse(is.na(x),"",x)))
  print.data.frame(format(df, justify="left"), quote=FALSE, row.names=FALSE)
}
