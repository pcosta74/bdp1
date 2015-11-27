# Cond. Prob:  P(A|B) = P(A&B)/P(B)
# Bayes Theor: P(B|A) = (P(B)*P(A|B))/P(A)
# NaiveBayes: argmax P(Ck) PRODi=1..n P(xi|Ck)

nb.classifier<-function(dataframe, classcol) {

  if(!classcol %in% names(dataframe))
    stop("Unknown column: ", classcol)
  
  list.attrs <-names(dataframe)
  if(all(list.attrs == classcol))
    stop("Empty attribute list")
  
  classifier<-sapply(list.attrs, nb.distr.table, classcol, dataframe) 

  if(length(classifier) == 0)
    stop("Unable to create classifier")
  
  return(classifier)
}

nb.predictor<-function(classifier, dataframe, classcol, log.probs=TRUE, pred.col=FALSE) {
  
  list.attrs<-unique(c(names(dataframe), classcol))
  
  if(all(sort(names(classifier)) != sort(list.attrs)))
    stop("Trainning and test dataframes do not match")
  
  dataframe[[classcol]]<-apply(dataframe, 1, function(row) {
    post.prob<-sapply(list.attrs, function(attr) {
      prob.tbl<-classifier[[attr]]
      if(attr == classcol) return(prob.tbl)
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
    })
    if(log.probs) post.prob<-apply(post.prob, 1, function(r) sum(log(r)))
    else post.prob<-apply(post.prob, 1, prod)
    return(names(which.max(post.prob)))
  }) 

  if(pred.col)
    return(dataframe[[classcol]])
  return(dataframe)
}

nb.distr.table<-function(attr, classcol, dataframe) {

  v.attrs<-unique(c(attr,classcol))
  n.rows<-nrow(dataframe)

  tbl<-table(dataframe[,v.attrs], dnn=v.attrs)
  
  # for faster calculation later on
  if(length(v.attrs) == 2)
    tbl<-addmargins(tbl,1,FUN=list(list("[TOTAL]"=sum,"[UNKNOWN]"=function(x) return(0))))

  # laplace correction
  tbl<-(tbl + 1/n.rows) / (n.rows + nrow(tbl)/n.rows)
  return(tbl)
}