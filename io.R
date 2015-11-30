source('common.R')

# Read a CVS file into a data frame
# path - path to the CVS file
# handle.blanks - how to handle lines with blank cells { 0-raise error, 1-skip lines, 2-use most commons values in column }
read.data.frame <- function(path, handle.blanks = 2) {
  dataframe <- read.csv(path,header = TRUE, na.strings = c("",NA), strip.white = TRUE)
  
  if (is.empty.data.frame(dataframe))
    stop("Empty data frame ", basename(path))
  
  if (!is.nominal.attr(dataframe))
    stop("Invalid data frame ", basename(path))
  
  cells.NA <- which(is.na(dataframe), arr.ind = TRUE)
  
  if(length(cells.NA)>0) {
    msg<-paste(apply(cells.NA,1,function(r) paste(LETTERS[r["col"]],r["row"],sep="")), collapse = "; ")
    
    switch(as.character(handle.blanks),
       "1" = {
          warning("\tEmpty cell(s) found: ", msg, "\n\tIgnoring lines", immediate. = TRUE)
          dataframe <- na.omit(dataframe)
        }, 
        "2" = {
          warning("\tEmpty cell(s) found: ", msg, "\n\tUsing most common values in columns" ,immediate. = TRUE)
          common.vals<-sapply(dataframe, function(x) {
            s<-summary(x[!is.na(x)])
            return(names(which.max(s[!is.na(names(s))])))
          })
          for(col in colnames(dataframe))
            dataframe[which(is.na(dataframe[,col])),col]<-common.vals[col]
        }, 
        {
          stop("\tEmpty cell(s) found: ", msg, "\n\tAborting!")
        }
    )
  }
  
  message("read ",nrow(dataframe), " rows from '",path,"'")
  return(dataframe)
}

# write the data frame to a file
# dataframe - the dataframe to write
# path - the file to write to
write.data.frame <- function(dataframe,path = "") {
  write.csv(dataframe, file = path, row.names = FALSE)
  if(path!="")
    message("writen ",nrow(dataframe), " rows to '",path,"'")
}
