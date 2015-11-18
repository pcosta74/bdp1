source('common.R')

# Read a CVS file into a data frame
# path - path to the CVS file
# handle.blanks - how to handle lines with blank cells { 0-raise error, 1-skip lines, 2-use most commons values in column }
read.data.frame <- function(path, handle.blanks = 0) {
  dataframe <- read.csv(path,header = TRUE, na.strings = c("",NA), strip.white = TRUE)
  
  if (is.empty.data.frame(dataframe))
    stop("Empty data frame ", basename(path))
  
  if (!is.nominal.attr(dataframe))
    stop("Invalid data frame ", basename(path))
  
  cells.NA <- which(is.na(dataframe), arr.ind = TRUE)
  if(length(cells.NA)>0) {
    msg<-paste(apply(cells.NA,1,function(r) paste(LETTERS[r["col"]],r["row"],sep="")), collapse = "; ")
    
    if(handle.blanks == 1) {
      warning("Empty cell(s) found: ", msg, "\nIgnoring lines", immediate. = TRUE)
      dataframe <- na.omit(dataframe)
    } 
    else if(handle.blanks == 2) {
      warning("Empty cell(s) found: ", msg, "\nUsing most common values in columns" ,immediate. = TRUE)
      common.vals<-sapply(dataframe, function(x) {
        s<-summary(x[!is.na(x)])
        return(names(which.max(s[!is.na(names(s))])))
      })
      for(col in colnames(dataframe))
        dataframe[which(is.na(dataframe[,col])),col]<-common.vals[col]
    } 
    else {
      stop("Empty cell(s) found: ", msg, "\nAborting!")
    }
  }

  return(dataframe)
}

# write the data frame to a file
# dataframe - the dataframe to write
# path - the file to write to
write.data.frame <- function(dataframe,path = "") {
  write.csv(dataframe, file = path, row.names = FALSE)
}

print.train.info <- function(train.df, pred.df, classifier, classcol) {
  cat("INFO\n")
  columns<-names(train.df)
  
  lbs <- c("relation:","instances:","attributes:","")
  vls <-c(substr(config$train.file,1,regexpr("\\.",config$train.file) - 1),
          nrow(train.df), ncol(train.df),
          paste(columns, collapse = ", "))
  df <- data.frame(lbs,vls,check.rows = TRUE)
  colnames(df) <- c(" "," ")
  print(df, row.names = FALSE, right = FALSE)
  cat("\n")

  mtx<-Reduce(rbind,classifier[columns!=classcol])
  mtx<-rbind(classifier[[classcol]],mtx)
  col0<-c(classcol, unlist(sapply(columns[columns!=classcol],
                     function(c,l) c(c,rep("",nrow(l[[c]])-1)), 
                     classifier, USE.NAMES=FALSE)))
  col1<-rownames(mtx)
  rownames(mtx)<-seq(1:nrow(mtx))
  df<-format(data.frame(col0,col1,mtx),justify="left")
  names(df)<-c("CLASS","",names(df)[3:ncol(df)])
  print.data.frame(df,quote=FALSE,row.names=FALSE)
  cat("\n")
  
  cat("Confusion matrix\n")
  print(table(train.df[[classcol]], pred.df[[classcol]], 
              dnn = list("value","prediction")))
  cat("\n")
}

