nb.classifier<-function(dataframe, classcol) {

  if(!classcol %in% names(dataframe))
    stop("Unknown column: ", classcol)
  
  list.attrs <-names(dataframe)
  if(all(list.attrs == classcol))
    stop("Empty attribute list")
  
  class.freq<-as.matrix(table(dataframe[classcol]))
  classifier<-sapply(list.attrs[list.attrs!=classcol], 
    function(attr) {
      card<-length(levels(dataframe[[attr]]))
      smp<-nb.distr.table(dataframe, list(attr), attr.card=card)
      jnt<-nb.distr.table(dataframe, list(classcol, attr), 
                         attr.card=card, class.frequency=class.freq)
      return(list(simple=smp, joint=jnt))
    }, 
    simplify = FALSE, USE.NAMES=TRUE
  ) 
  classifier[[classcol]]<-nb.distr.table(
    dataframe, list(classcol), attr.card=nrow(class.freq))
  
  if(!length(classifier))
    stop("Unable to create classifier")
  
  return(classifier)
}

nb.predictor<-function(classifier, dataframe, classcol) {
  
  list.attrs<-unique(c(names(dataframe),classcol))
  is.valid<-all(sort(names(classifier)) == sort(list.attrs))
  if(!is.valid)
    stop("Trainning and test dataframes do not match")
  
  list.attrs<-list.attrs[list.attrs!=classcol]
  list.labels<-rownames(classifier[[classcol]])

  dataframe[[classcol]]<-NA
  post.probs<-matrix(0,2,1,dimnames=list(list.labels,classcol))

  for(k in 1:nrow(dataframe)) {
    row<-dataframe[k,]
    post.probs[,classcol]<-classifier[[classcol]]

    for(attr in list.attrs) {
      index<-as.character(row[[attr]])
      prob.tables<-classifier[[attr]]
      post.probs[, classcol] <- post.probs[, classcol] *
        as.matrix(prob.tables$joint[,index]/prob.tables$simple[index,])
    }
    
    # Irrelevant
    #if(sum(post.probs)!=0)
    #  post.probs<-post.probs/sum(post.probs)
    
    label<-subset(post.probs,post.probs==max(post.probs))
    dataframe[[k,classcol]]<-rownames(label)
  }
  
  return(dataframe)
}

nb.distr.table<-function(dataframe, list.attrs, attr.card, class.frequency=NULL) {
  
  table<-table(dataframe[unlist(list.attrs)],dnn=list.attrs)
  prob.table<-as.matrix(table, responseName="probability")

  if(length(list.attrs)==1)
    colnames(prob.table)<-list.attrs

  dataframe.size<-nrow(dataframe)
  if(is.null(class.frequency))
    size<-dataframe.size
  else
    size<-class.frequency
  
  if(any(size == 0))
    stop("invalid data size: ",size)
  
  #laplace correction
  if(is.numeric(attr.card)) {
    prob.table<-prob.table + 1/dataframe.size
    size<-size + attr.card/dataframe.size
  } 
  
  if(class(size) == "matrix")
    prob.table<-prob.table/size[,1]
  else
    prob.table<-prob.table/size

  return(prob.table)  
}


nb.print.stats<-function(classifier) {
}