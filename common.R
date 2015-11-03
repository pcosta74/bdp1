is.nominal.attribute<-function(attr,warn=T) {
  attr.type<-sapply(attr,class)
  non.nominal<-attr.type!="character"
  
  if(any(non.nominal)) {
    message<-paste(attr[non.nominal],"[",attr.type[non.nominal],"]",
                   sep="",collapse=",")
    warning("Non-nominal attributes:", message)
    return(FALSE)
  }
  
  return(TRUE)
}

is.empty.data.frame<-function(data.frame) {
  if(!is.data.frame(data.frame))
    stop("Expected data.frame, got ",sapply(data.frame,class))
  
  return (length(data.frame) < 1)
}
