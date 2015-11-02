source('common.R')

nb.classifier<-function(classcolumn,dataset) {
  if(is.empty.data.frame(dataset))
    stop(paste("Empty data frame"))
  
  if(!classcolumn %in% names(dataset))
    stop(paste("Unknown column: ", classcolumn))
  
  classifier<-list()
  classifier[[classcolumn]]<-nb.distribution_table(
    dataset, list(classcolumn))

  list.attrs <-names(dataset)[names(dataset)!=classcolumn]
  if(!length(list.attrs))
    stop("Empty attribute list")
  
  for(attr in list.attrs) {
    classifier[[attr]]<-nb.distribution_table(
      dataset, list(classcolumn,attr))
  }
  
  if(!length(classifier))
    stop("Unable to create classifier")
  
  return(classifier)
}

nb.predictor<-function(classifier,classcolumn,dataset) {
  if(is.empty.data.frame(dataset))
    stop(paste("Empty data frame"))
  
  if(!classcolumn %in% names(dataset))
    stop(paste("Unknown column: ", classcolumn))

  dataset[[classcolumn]]<-NA
  list.attrs<-names(dataset)[names(dataset)!=classcolumn]
  list.labels<-rownames(classifier[[classcolumn]])
  
  for(k in 1:nrow(dataset)) {
    row<-dataset[k,]
    postprob<-matrix(0,2,1,dimnames=list(list.labels,classcolumn))
  
    for(label in rownames(classifier[[classcolumn]])) {
      postprob[label,]<-as.double(classifier[[classcolumn]][label,])
      for(attr in list.attrs) {
        postprob[label,]<-
          postprob[label,] * as.double(classifier[[attr]][label,row[[attr]]])
      }
    }
    
    if(sum(postprob)!=0)
      postprob<-postprob/sum(postprob)
    
    cls<-subset(postprob,postprob==max(postprob))
    dataset[[k,classcolumn]]<-rownames(cls)
  }
  
  return(dataset)
}

nb.distribution_table<-function(dataset, list.attrs=list()) {
  if(!is.list(list.attrs))
    stop(paste("Expected list, got ",sapply(list.attrs,class)))  
  
  if(is.nominal.attribute(list.attrs)) {
    table<-table(dataset[unlist(list.attrs)],dnn=list.attrs)
    prob.table<-as.matrix(table, responseName="probability")
    prob.table<-prob.table/nrow(dataset)
    return(prob.table)  
  }

  return(NULL)
}
