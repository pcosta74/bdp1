read.dataset<-function(path) {
  dataset<-read.csv(path,header=T)
  
  rows.empty.cells<-unique(which(dataset=="", arr.ind=TRUE)[,"row"])
  if(length(rows.empty.cells)>0) {
    warning("Empty cell(s) found, ignoring line(s): ",
            paste(rows.empty.cells,collapse=","), immediate. = TRUE)
    dataset<-dataset[-rows.empty.cells,]
  }
  
  return(dataset)
}

write.dataset<-function(dataset,path="") {
  write.csv(dataset, file=path, row.names = F)
}
