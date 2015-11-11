source('common.R')

nb.classifier<-function(classcolumn,dataset) {
  if(is.empty.data.frame(dataset))
    stop("Empty data frame")
  
  if(!classcolumn %in% names(dataset))
    stop("Unknown column: ", classcolumn)
  
  list.attrs <-names(dataset)[names(dataset)!=classcolumn]
  if(!length(list.attrs))
    stop("Empty attribute list")
  
  class.freq<-as.matrix(table(dataset[classcolumn]))
  classifier<-sapply(list.attrs, 
    function(attr) {
      card<-length(levels(dataset[[attr]]))
      smp<-nb.distr.table(dataset, list(attr), attr.card=card)
      jnt<-nb.distr.table(dataset, list(classcolumn, attr), 
                         attr.card=card, class.frequency=class.freq)
      return(list(simple=smp, joint=jnt))
    }, 
    simplify = FALSE, USE.NAMES=TRUE
  ) 
  classifier[[classcolumn]]<-nb.distr.table(
    dataset, list(classcolumn), attr.card=nrow(class.freq))
  
  if(!length(classifier))
    stop("Unable to create classifier")
  
  return(classifier)
}

nb.predictor<-function(classifier, classcolumn, dataset) {
  if(is.empty.data.frame(dataset))
    stop("Empty data frame")
  
  list.attrs<-unique(c(names(dataset),classcolumn))
  is.valid<-all(sort(names(classifier)) == sort(list.attrs))
  if(!is.valid)
    stop("Trainning and test datasets do not match")
  
  list.attrs<-list.attrs[list.attrs!=classcolumn]
  list.labels<-rownames(classifier[[classcolumn]])

  dataset[[classcolumn]]<-NA
  post.probs<-matrix(0,2,1,dimnames=list(list.labels,classcolumn))

  for(k in 1:nrow(dataset)) {
    row<-dataset[k,]
    post.probs[,classcolumn]<-classifier[[classcolumn]]

    for(attr in list.attrs) {
      index<-as.character(row[[attr]])
      prob.tables<-classifier[[attr]]
      post.probs[, classcolumn] <- post.probs[, classcolumn] *
        as.matrix(prob.tables$joint[,index]/prob.tables$simple[index,])
    }
    
    # Irrelevant
    #if(sum(post.probs)!=0)
    #  post.probs<-post.probs/sum(post.probs)
    
    label<-subset(post.probs,post.probs==max(post.probs))
    dataset[[k,classcolumn]]<-rownames(label)
  }
  
  return(dataset)
}

nb.distr.table<-function(dataset, list.attrs, attr.card, class.frequency=NULL) {
  
  if(!is.nominal.attribute(list.attrs))
    return(NULL)
  
  table<-table(dataset[unlist(list.attrs)],dnn=list.attrs)
  prob.table<-as.matrix(table, responseName="probability")

  if(length(list.attrs)==1)
    colnames(prob.table)<-list.attrs

  dataset.size<-nrow(dataset)
  if(is.null(class.frequency))
    size<-dataset.size
  else
    size<-class.frequency
  
  if(any(size == 0))
    stop("invalid data size: ",size)
  
  #laplace correction
  if(is.numeric(attr.card)) {
    prob.table<-prob.table + 1/dataset.size
    size<-size + attr.card/dataset.size
  } 
  
  if(class(size) == "matrix")
    prob.table<-prob.table/size[,1]
  else
    prob.table<-prob.table/size

  return(prob.table)  
}


nb.print.stats<-function(classifier) {
}