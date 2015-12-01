# Cond. Prob:  P(A|B) = P(A&B)/P(B)
# Bayes Theor: P(B|A) = (P(B)*P(A|B))/P(A)
# NaiveBayes: argmax P(Ck) PRODi=1..n P(xi|Ck)

naivebayes<-function(relation, formula, train.data = data.frame(), test.data = data.frame()) {
  
  t<-terms(formula, data=train.data, keep.order = TRUE)
  classcol<-rownames(attr(t,"factors"))[attr(t,"response")]
  list.attrs<-attr(t,"term.labels")

  if(!classcol %in% names(train.data))
    stop("Unknown class column: ", classcol)
  
  if(!all(list.attrs %in% names(test.data)))
    stop("Trainning and test dataframes do not match")
  
  list.attrs <-unique(c(classcol, list.attrs))
  if(all(list.attrs == classcol))
    stop("Empty attribute list")
  
  classifier<-nb.classifier(classcol, list.attrs, train.data)
  
  pred.data<-nb.predictor(classifier, classcol, list.attrs, train.data)
  conf.matrix<-table(train.data[[classcol]], pred.data[[classcol]], 
                      dnn = list("value","prediction"))
  
  nb.print.train.info(relation, train.data, classcol, conf.matrix, classifier)
  
  pred.data<-nb.predictor(classifier, classcol, list.attrs, test.data)
  nb.print.predict.info(pred.data, classcol)
  
  return(pred.data)
}

#
nb.classifier<-function(classcol, list.attrs, data) {

  classifier<-sapply(list.attrs, nb.distr.table, classcol, data) 

  if(length(classifier) == 0)
    stop("Unable to create classifier")
  
  return(classifier)
}

#
nb.predictor<-function(classifier, classcol, list.attrs, data, pred.col=FALSE) {
  
  data[[classcol]]<-apply(data, 1, function(row) {
    post.prob<-sapply(list.attrs, nb.cond.prob, classcol, classifier, row)
    
    # Standard definition
    # post.prob<-apply(post.prob, 1, prod)
    
    # Additive approach (sum of logs)
    post.prob<-apply(post.prob, 1, function(r) sum(log(r)))
    
    return(names(which.max(post.prob)))
  }) 

  if(pred.col)
    return(data[[classcol]])

  return(data)
}

nb.distr.table<-function(attr, classcol, data) {

  v.attrs<-unique(c(attr,classcol))
  n.rows<-nrow(data)
  
  tbl<-table(data[,v.attrs], dnn=v.attrs)
  
  # for faster calculation later on
  if(length(v.attrs) == 2)
    tbl<-addmargins(tbl,1,FUN=list(list("[TOTAL]"=sum,"[UNKNOWN]"=function(x) return(0))))
  
  # laplace correction
  # tbl<-(tbl + 1/n.rows) / (n.rows + nrow(tbl)/n.rows)

  tbl <- tbl /n.rows
  
  return(tbl)
}

nb.cond.prob<-function(attr, classcol, classifier, row) {
  prob.tbl<-classifier[[attr]]
  
  if(attr == classcol) 
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

nb.print.predict.info<-function(pred.df, classcol) {

  cat("=== Prediction information ===\n")
  t<-table(pred.df[[classcol]])
  t<-rbind(t,prop.table(t))
  rownames(t)<-c("Frequency","Percent")
  print(t)
  
  cat("\n\n")
}

nb.print.train.info <- function(relation, train.df, classcol, conf.matrix, classifier) {
  
  cat("=== Run information ===\n")
  nb.print.relation.info(relation, train.df)
  
  cat("\n\n=== Evaluation model ===\n\n")
  nb.print.classifier(classifier, classcol)
    
  cat("\n\n=== Summary ===\n")  
  nb.print.cm.summary(conf.matrix)
    
  cat("\n\n=== Detailed accuracy by class ===\n\n")
  nb.print.cm.accuracy(conf.matrix)
  
  cat("\n\n=== Confusion matrix===\n\n")
  print(conf.matrix)
  
  cat("\n\n")
}

nb.print.relation.info<-function(relation, data) {
  df <- data.frame(
    c("relation:","instances:","attributes:",""),
    c(relation, nrow(data), ncol(data), paste(names(data), collapse = ", ")),
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
      REC=tp/sum(tp,fn), #recall
      PPV=tp/sum(tp,fp), #precision
      TNR=tn/sum(fp,tn), #specificity
      #NVP=tn/sum(tn,fn),
      #FDR=fp/sum(fp,tp),
      #FNR=fn/sum(fn,tp), #miss-rate
      F1S=(2*tp)/sum(2*tp,fp,fn),
      ACC=sum(tp,tn)/sum(tp,fp,fn,fp))
  }, cm))
  wavg<-apply(apply(cm,1,sum)*mtx,2,sum)/sum(cm)
  mtx<-signif(rbind(mtx,c(wavg)), digits = 5)
  colnames(mtx)<-c("TP Rate","FP Rate", "Recall", "Precision", "Specificity", "F1 Score", "Accuracy")
  print(data.frame(mtx,row.names=c(rownames(cm),"Weighted Avg.")))  
}

nb.print.classifier<-function(classifier, classcol) {
  ndx<-which(names(classifier) == classcol)
  
  lst<-sapply(classifier[-ndx], function(t) return(head(t, -1)))
  
  mtx<-Reduce(function(x,y) rbind(x,rep(NA,ncol(x)),y), lst)
  mtx<-rbind(classifier[[ndx]], rep(NA,ncol(mtx)), mtx)
  mtx<-round(addmargins(mtx,2,FUN=list("TOTAL"=sum)),5)

  lst<-sapply(names(lst), function(c,l) c(c,rep("",nrow(l[[c]]))), lst, 
              USE.NAMES=FALSE)
  names<-mapply(paste, c("", unlist(lst)), rownames(mtx),sep="  ")
  rownames(mtx)<-seq(1:nrow(mtx))
  
  df<-data.frame(CLASS=names, mtx)
  df<-as.data.frame(apply(df,2,function(x) ifelse(is.na(x),"",x)))
  print.data.frame(format(df, justify="left"), quote=FALSE, row.names=FALSE)
}