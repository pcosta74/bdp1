source('common.R')

read.data.frame<-function(path) {
  
  dataframe<-read.csv(path,header=T)
  
  if(is.empty.data.frame(dataframe))
    stop("Empty data frame ", basename(path))
  
  if(!is.nominal.attr(dataframe))
    stop("Invalid data frame ", basename(path))
  
  rows.empty.cells<-which(dataframe=="", arr.ind=TRUE)[,"row"]
  if(length(rows.empty.cells)>0) {
    warning("Empty cell(s) found, ignoring line(s): ",
            paste(sort(unique(rows.empty.cells)),collapse=","),
            immediate. = TRUE)
    dataframe<-dataframe[-rows.empty.cells,]
  }
  
  return(dataframe)
}

write.data.frame<-function(dataframe,path="") {
  write.csv(dataframe, file=path, row.names = F)
}
