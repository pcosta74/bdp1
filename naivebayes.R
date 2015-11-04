source('common.R')

nb.classifier<-function(classcolumn,dataset) {
  if(is.empty.data.frame(dataset))
    stop("Empty data frame")
  
  if(!classcolumn %in% names(dataset))
    stop("Unknown column: ", classcolumn)
  
  classifier<-list()
  classifier[[classcolumn]]<-nb.distribution_table(
    dataset, list(classcolumn))

  list.attrs <-names(dataset)[names(dataset)!=classcolumn]
  if(!length(list.attrs))
    stop("Empty attribute list")
  
  for(attr in list.attrs) {
    classifier[[attr]]<-list(
      simple=nb.distribution_table(dataset, list(attr)),
      join=nb.distribution_table(dataset, list(classcolumn,attr))
    )
  }
  
  if(!length(classifier))
    stop("Unable to create classifier")
  
  return(classifier)
}

nb.predictor<-function(classifier,classcolumn,dataset) {
  if(is.empty.data.frame(dataset))
    stop("Empty data frame")
  
  if(!classcolumn %in% names(dataset))
    stop("Unknown column: ", classcolumn)

  list.attrs<-unique(c(names(dataset),classcolumn))
  is.valid<-all(sort(names(classifier)) == sort(list.attrs))
  if(!is.valid)
    stop("Trainning and test datasets do not match")
  
  list.attrs<-list.attrs[list.attrs!=classcolumn]
  list.labels<-rownames(classifier[[classcolumn]])

  dataset[[classcolumn]]<-NA
  postprob<-matrix(0,2,1,dimnames=list(list.labels,classcolumn))

  for(k in 1:nrow(dataset)) {
    row<-dataset[k,]
    postprob[,classcolumn]<-classifier[[classcolumn]]

    for(attr in list.attrs) {
      simple<-classifier[[attr]][["simple"]]
      join<-classifier[[attr]][["join"]]
      postprob[, classcolumn] <- postprob[, classcolumn] *
        t(t(join[,row[[attr]]]/simple[row[[attr]]]))
    }
    
    # Irrelevant?
    #if(sum(postprob)!=0)
    #  postprob<-postprob/sum(postprob)
    
    label<-subset(postprob,postprob==max(postprob))
    dataset[[k,classcolumn]]<-rownames(label)
    postprob[,classcolumn]=0
  }
  
  return(dataset)
}

nb.distribution_table<-function(dataset, list.attrs=list()) {
  if(!is.list(list.attrs))
    stop("Expected list, got ",sapply(list.attrs,class))  
  
  if(is.nominal.attribute(list.attrs)) {
    table<-table(dataset[unlist(list.attrs)],dnn=list.attrs)
    
    prob.table<-as.matrix(table, responseName="probability")
    prob.table<-prob.table/nrow(dataset)
    
    if(length(list.attrs)==1)
      colnames(prob.table)<-list.attrs
    
    return(prob.table)  
  }

  return(NULL)
}
