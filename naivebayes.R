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
  
  freq<-as.matrix(table(dataset[classcolumn]))
  for(attr in list.attrs) {
    sp<-nb.distribution_table(dataset, list(attr))
    jp<-nb.distribution_table(dataset, list(classcolumn,attr),freq)
    classifier[[attr]]<-list(simple=sp,join=jp)
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
        as.matrix(join[,row[[attr]]]/simple[row[[attr]]])
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

nb.distribution_table<-function(dataset, list.attrs=list(), size=nrow(dataset)) {
  if(!is.list(list.attrs))
    stop("Expected list, got ",sapply(list.attrs,class))  

  if(is.nominal.attribute(list.attrs)) {
    table<-table(dataset[unlist(list.attrs)],dnn=list.attrs)
    
    prob.table<-as.matrix(table, responseName="probability")
    if(class(size) == "integer")
        prob.table<-prob.table/size
    else if(class(size) == "matrix")
      prob.table<-prob.table/size[,1]
    else
      stop("invalid data type: size [", class(size),"]")
    
    if(length(list.attrs)==1)
      colnames(prob.table)<-list.attrs
    
    return(prob.table)  
  }

  return(NULL)
}


nb.print.stats<-function(classifier) {
  t<-as.table(t(classifier[[1]]))
  rownames(t)<-c("")
  print("A priori probabilities:\n")
  print(t)
}