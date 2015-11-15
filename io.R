source('common.R')

read.data.frame <- function(path, skip.blanks = TRUE) {
  dataframe <- read.csv(path,header = TRUE)
  
  if (is.empty.data.frame(dataframe))
    stop("Empty data frame ", basename(path))
  
  if (!is.nominal.attr(dataframe))
    stop("Invalid data frame ", basename(path))
  
  if(skip.blanks) {
    rows.empty.cells <- which(dataframe == "", arr.ind = TRUE)[,"row"]
    if (length(rows.empty.cells) > 0) {
      warning("Empty cell(s) found, ignoring line(s): ",
              paste(sort(unique(rows.empty.cells)),collapse = ","),
              immediate. = TRUE)
      dataframe <- dataframe[-rows.empty.cells,]
    }
  }
  
  return(dataframe)
}

write.data.frame <- function(dataframe,path = "") {
  write.csv(dataframe, file = path, row.names = FALSE)
}

print.train.info <- function(dataframe, classifier, classcol) {
  cat("RUN INFO")
  lbs <- c("relation:","instances:","attributes:","")
  vls <-c(substr(config$train.file,1,regexpr("\\.",config$train.file) - 1),
          nrow(dataframe), ncol(dataframe),
          paste(names(dataframe), collapse = ", "))
  df <- data.frame(lbs,vls,check.rows = TRUE)
  colnames(df) <- c(" "," ")
  print(df, row.names = FALSE, right = FALSE)
  cat("\n")
  
  #as.data.frame(sapply(classifier,function(ndx) print(format(ndx,nsmall = 10))))
}
