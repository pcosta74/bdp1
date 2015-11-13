is.nominal.attr<-function(x) {
  attr.type<-apply(x, 1, class)
  non.nominal<-attr.type!="character"
  
  if(any(non.nominal)) {
    message<-paste(x[non.nominal],"[",attr.type[non.nominal],"]",
                   sep="",collapse=",")
    warning("Non-nominal attributes:", message, immediate.=T)
    return(FALSE)
  }
  return(TRUE)
}

is.empty.data.frame<-function(data.frame) {
  if(!is.data.frame(data.frame))
    stop("Expected data.frame, got ",sapply(data.frame,class))
  
  return (length(data.frame) < 1)
}
