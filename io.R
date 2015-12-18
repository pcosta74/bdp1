# Read a CVS file into a data frame
# path - path to the CVS file
# handle.blanks - how to handle lines with blank cells { 0-raise error, 1-skip lines, 2-use most commons values in column }
read.data.frame <- function(path, blank.strings = NULL, header = TRUE, sep = ",", quote = "\"", dec = ".") {
  if(is.character(blank.strings) | is.vector(blank.strings))
    blank.strings<-unique(c(blank.strings,NA))
  
  dataframe <- read.csv(path, header=header, sep=sep, quote=quote, dec=dec, na.strings = blank.strings, strip.white = TRUE)
  
  if(!is.data.frame(dataframe))
    stop("Expected data.frame, got ",sapply(dataframe,class))
  
  if (!length(dataframe))
    stop("Empty data frame ", basename(path))

  cells.NA<-colSums(is.na(dataframe)) == nrow(dataframe)
  if(any(cells.NA)) {
    warning("\tBlank column(s) found, discarding: ",paste(names(which(cells.NA)),collapse=", "),immediate.=TRUE)
    dataframe<-dataframe[!cells.NA]
  }
  
  cells.NA <- which(is.na(dataframe), arr.ind = TRUE)
  if(length(cells.NA)>0) {
    msg<-paste(apply(cells.NA,1,function(r) paste(LETTERS[r["col"]],r["row"],sep="")), collapse = "; ")
    
    if(is.null(blank.strings)) {
       warning("\tEmpty cell(s) found: ", msg, "\n\tIgnoring lines", immediate. = TRUE)
       dataframe <- na.omit(dataframe)
    } else {
      warning("\tEmpty cell(s) found: ", msg, "\n\tUsing most common values in columns" ,immediate. = TRUE)  
      common.vals<-apply(dataframe,2,function(c) c[which.max(summary(factor(c[!is.na(c)])))])
      for(k in unique(cells.NA[,"col"]))
        if(is.null(levels(dataframe[,k])))
          dataframe[,k][is.na(dataframe[,k])]<-as.numeric(common.vals[k]) 
        else 
          dataframe[,k][is.na(dataframe[,k])]<-common.vals[k]
    }
  }
  
  filename<-basename(path)
  attr(dataframe, "relation")<-substr(filename,1,regexpr("\\.",filename) - 1)

  message("read ",nrow(dataframe), " rows from '",path,"'")
  return(dataframe)
}  

# write the data frame to a file
# dataframe - the dataframe to write
# path - the file to write to
write.data.frame <- function(dataframe,path = "") {
  if(!is.null(path) && path!="") {
    write.csv(dataframe, file = path, row.names = FALSE)
    message("writen ",nrow(dataframe), " rows to '",path,"'")
  }
  else {
    cat("=== Result dataset ===\n\n")
    print(dataframe, row.names=FALSE)
  }
}
