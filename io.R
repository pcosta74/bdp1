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
  columns<-names(train.df)
  
  cat("=== Run information ===\n")
  lbs <- c("relation:","instances:","attributes:","")
  vls <-c(substr(config$train.file,1,regexpr("\\.",config$train.file) - 1),
          nrow(train.df), ncol(train.df),
          paste(columns, collapse = ", "))
  df <- data.frame(lbs,vls,check.rows = TRUE)
  colnames(df) <- c(" "," ")
  print(df, row.names = FALSE, right = FALSE)

  cat("\n\n=== Evaluation model ===\n\n")
  mtx<-Reduce(function(x,y) rbind(x,rep(NA,ncol(x)),y), 
              classifier[columns!=classcol])
  mtx<-round(rbind(classifier[[classcol]], rep(NA,ncol(mtx)), mtx), digits=5)
  names<-c("", unlist(sapply(columns[columns!=classcol],
                      function(c,l) c(c,rep("",nrow(l[[c]]))), 
                      classifier, USE.NAMES=FALSE)))
  names<-mapply(paste, names, rownames(mtx),sep="  ")
  rownames(mtx)<-seq(1:nrow(mtx))
  df<-format(data.frame(CLASS=names, mtx), justify="left")
  df<-as.data.frame(apply(df,2,function(x) ifelse(sub("\\s+","",x)=="NA","",x)))
  print.data.frame(df, quote=FALSE, row.names=FALSE)

  cm<-table(train.df[[classcol]], pred.df, 
            dnn = list("value","prediction"))
  
  cat("\n\n=== Summary ===\n\n")  
  total<-sum(cm)
  mdiag<-sum(diag(cm))
  df<-data.frame(c("Correctly classified:","Incorrectly classified:","Accuracy rate:","Error rate:","Number of instances:"),
                 c(mdiag,total-mdiag, signif(mdiag/total,5), signif(1-(mdiag/total),5),total))
  colnames(df)<-c(" "," ")
  print(format(df,justify = "left"),row.names=FALSE)  
  
  cat("\n\n=== Detailed acuracy by class ===\n\n")
  m<-t(sapply(seq_along(diag(cm)),function(n,cm) {
    tp<-cm[n,n]
    fn<-sum(cm[n,-n])
    fp<-sum(cm[-n,n])
    tn<-sum(cm[-n,-n])
    c(TPR=tp/sum(tp,fn), #sensitivity
      FPR=fp/sum(fp,tn), #fall-out
      PPV=tp/sum(tp,fp), #precision
      TNR=tn/sum(fp,tn), #specificity
      #NVP=tn/sum(tn,fn),
      #FDR=fp/sum(fp,tp),
      #FNR=fn/sum(fn,tp), #miss-rate
      F1S=(2*tp)/sum(2*tp,fp,fn),
      ACC=sum(tp,tn)/sum(tp,fp,fn,fp))
  }, cm))
  wavg<-apply(apply(cm,1,sum)*m,2,sum)/total
  m<-signif(rbind(m,c(wavg)), digits = 5)
  colnames(m)<-c("TP Rate","FP Rate", "Precision", "Specificity", "F1 Score", "Accuracy")
  print(data.frame(m,row.names=c(rownames(cm),"Weighted Avg.")))

  cat("\n\n=== Confusion matrix===\n\n")
  print(cm)
}

