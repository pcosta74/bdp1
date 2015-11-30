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
  nb.print.train.info(relation, train.data, pred.data, classifier, classcol)
  
  pred.data<-nb.predictor(classifier, classcol, list.attrs, test.data)
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
nb.predictor<-function(classifier, classcol, list.attrs, data, log.probs=TRUE, pred.col=FALSE) {
  
  data[[classcol]]<-apply(data, 1, function(row) {
    post.prob<-sapply(list.attrs, nb.cond.prob, classcol, classifier, row)
    
    if(log.probs)
      post.prob<-apply(post.prob, 1, function(r) sum(log(r)))
    else 
      post.prob<-apply(post.prob, 1, prod)
    
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
  tbl<-(tbl + 1/n.rows) / (n.rows + nrow(tbl)/n.rows)
  
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


nb.print.train.info <- function(relation, train.df, pred.df, classifier, classcol) {
  
  cat("=== Run information ===\n")
  lbs <- c("relation:","instances:","attributes:","")
  vls <-c(relation,
          nrow(train.df), ncol(train.df),
          paste(names(train.df), collapse = ", "))
  df <- data.frame(lbs,vls,check.rows = TRUE)
  colnames(df) <- c(" "," ")
  print(df, row.names = FALSE, right = FALSE)
  
  cat("\n\n=== Evaluation model ===\n\n")
  lst<-sapply(classifier[names(classifier) != classcol], function(t) return(head(t,-1)))
  mtx<-Reduce(function(x,y) rbind(x,rep(NA,ncol(x)),y), lst)
  mtx<-rbind(classifier[[classcol]], rep(NA,ncol(mtx)), mtx)
  names<-c("", unlist(sapply(names(lst),
                             function(c,l) c(c,rep("",nrow(l[[c]]))), 
                             lst, USE.NAMES=FALSE)))
  names<-mapply(paste, names, rownames(mtx),sep="  ")
  rownames(mtx)<-seq(1:nrow(mtx))
  df<-format(data.frame(CLASS=names, round(mtx,digits = 5)), justify="left")
  df<-as.data.frame(apply(df,2,function(x) ifelse(sub("\\s+","",x)=="NA","",x)))
  print.data.frame(df, quote=FALSE, row.names=FALSE)
  
  cm<-table(train.df[[classcol]], pred.df[[classcol]], 
            dnn = list("value","prediction"))
  
  cat("\n\n=== Summary ===\n")  
  total<-sum(cm)
  mdiag<-sum(diag(cm))
  df<-data.frame(c("Correctly classified:","Incorrectly classified:","Accuracy rate:","Error rate:","Number of instances:"),
                 c(mdiag,total-mdiag, signif(mdiag/total,5), signif(1-(mdiag/total),5),total))
  colnames(df)<-c(" "," ")
  print(format(df,justify = "left"),row.names=FALSE)  
  
  cat("\n\n=== Detailed accuracy by class ===\n\n")
  m<-t(sapply(seq_along(diag(cm)),function(n,cm) {
    tp<-cm[n,n]
    fn<-sum(cm[n,-n])
    fp<-sum(cm[-n,n])
    tn<-sum(cm[-n,-n])
    c(TPR=tp/sum(tp,fn), #sensitivity
      FPR=fp/sum(fp,tn), #fall-out
      PPV=tp/sum(tp,fp), #precision
      TNR=tn/sum(fp,tn), #specificity
      #NVP=tn/sum(tn,fn),
      #FDR=fp/sum(fp,tp),
      #FNR=fn/sum(fn,tp), #miss-rate
      F1S=(2*tp)/sum(2*tp,fp,fn),
      ACC=sum(tp,tn)/sum(tp,fp,fn,fp))
  }, cm))
  wavg<-apply(apply(cm,1,sum)*m,2,sum)/total
  m<-signif(rbind(m,c(wavg)), digits = 5)
  colnames(m)<-c("TP Rate","FP Rate", "Precision", "Specificity", "F1 Score", "Accuracy")
  print(data.frame(m,row.names=c(rownames(cm),"Weighted Avg.")))
  
  cat("\n\n=== Confusion matrix===\n\n")
  print(cm)
}
