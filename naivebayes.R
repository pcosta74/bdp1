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
      sum.row<-nrow(prob.tbl)
      if(attr == classcol) return(prob.tbl)
      else if(!row[[attr]] %in% rownames(prob.tbl[-sum.row,])) return(c(0.0000001,0.0000001))
      else return(prob.tbl[-sum.row,][row[[attr]],]/prob.tbl[sum.row,])
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

  # with laplace correction
  tbl<-table(dataframe[,v.attrs], dnn=v.attrs) + 1/n.rows
  tbl<-tbl / (n.rows + nrow(tbl)/n.rows)

  # for faster calculation later on
  if(length(v.attrs) == 2) {
    tbl<-addmargins(tbl,1)
    rownames(tbl)[nrow(tbl)]<-c("[TOTAL]")
  }
    
  return(tbl)
}