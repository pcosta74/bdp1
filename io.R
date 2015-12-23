# Read a CVS file into a data frame
# path - path to the CVS file
read.data.frame <- function(path, na.strings = "", header = TRUE, sep = ",", quote = "\"", dec = ".") {
  
  time<-Sys.time()

  if(is.character(na.strings) | is.vector(na.strings))
    na.strings<-unique(c(na.strings,NA))
  
  path<-file.path(path)
  if(!file.exists(path))
    error("File does not exist: '",path,"'")
    
  dataframe<-read.csv(path, header=header, sep=sep, quote=quote, dec=dec, na.strings = na.strings, strip.white = TRUE)
  dataframe<-na.strip.data.frame(dataframe, is.null(na.strings))  

  if (!length(dataframe))
    stop("Empty data frame ", basename(path))

  filename<-basename(path)
  attr(dataframe, "relation")<-substr(filename,1,regexpr("\\.",filename) - 1)

  size<-object.size(dataframe)
  time<-sub("Time difference of ","",capture.output(Sys.time()-time))
  message("Read ",size, " from '",path,"' in ",time)
  
  return(dataframe)
}  

# write the data frame to a file
# dataframe - the dataframe to write
# path - the file to write to
write.data.frame <- function(dataframe,path = "") {
  if(is.null(path) | path=="") {
    cat("=== Result dataset ===\n\n")
    print(dataframe, row.names=FALSE)
  }
  else {
    time<-Sys.time()
    
    path<-file.path(path)
    if(!dir.exists(dirname(path)))
      dir.create(dirname(path),recursive=TRUE)
    
    write.csv(dataframe, file = path, row.names = FALSE)
    size<-object.size(dataframe)
    time<-sub("Time difference of ","",capture.output(Sys.time()-time))
    message("Wrote ",size, " to '",path,"' in ",time)
  }
}

#
write.plot <- function(path, device=png, width=600, height=600) {
  if(!is.null(path)) {

    path<-file.path(path)
    if(!dir.exists(dirname(path)))
      dir.create(dirname(path),recursive=TRUE)

    dev.copy(device, path, width=width, height=height)
    dev.off()
  }  
}

#
na.strip.data.frame <- function(dataframe, drop.lines=TRUE) {
  
  cells.NA<-colSums(is.na(dataframe)) == nrow(dataframe)
  if(any(cells.NA)) {
    warning("\tBlank column(s) found, discarding: ",paste(names(which(cells.NA)),collapse=", "),immediate.=TRUE)
    dataframe<-dataframe[!cells.NA]
  }
  
  cells.NA <- which(is.na(dataframe), arr.ind = TRUE)
  if(length(cells.NA)>0) {
    msg<-paste(apply(cells.NA,1,function(r) paste(LETTERS[r["col"]],r["row"],sep="")), collapse = "; ")
    
    if(drop.lines) {
      warning("\tEmpty cell(s) found: ", msg, "\n\tIgnoring lines", immediate. = TRUE)
      dataframe <- na.omit(dataframe)
    } else {
      warning("\tEmpty cell(s) found: ", msg, "\n\tUsing most common values in columns" ,immediate. = TRUE)  
      common.vals<-as.data.frame(lapply(dataframe,function(c) c[which.max(c[!is.na(c)])]))
      for(k in unique(cells.NA[,"col"]))
        dataframe[unique(cells.NA[,"row"]),k]<-common.vals[[k]]
    }
  }
  
  return(dataframe)
}
