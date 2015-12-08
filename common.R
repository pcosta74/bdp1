is.nominal.attr<-function(x) {
  attr.type<-apply(x, 2, class)
  nominal<-attr.type=="character"
  
  if(any(!nominal)) {
    msg<-paste(names(x[!nominal]),"[",attr.type[!nominal],"]",
               sep=" ",collapse=", ")
    warning("Non-nominal attributes:", msg, immediate.=T)
    return(FALSE)
  }
  return(TRUE)
}

is.empty.data.frame<-function(data.frame) {
  if(!is.data.frame(data.frame))
    stop("Expected data.frame, got ",sapply(data.frame,class))
  
  return (length(data.frame) < 1)
}

