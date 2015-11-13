source('io.R')
source('naivebayes.R')

config<<-list(
  train.file="clientes_train.csv",
  train.classcol="emprestimo",
  test.file="clientes_test2.csv",
  test.classcol="emprestimo",
  outputfile="clientes_pred2.csv"
)

run<-function() {
  train.df<-read.data.frame(config$train.file)
  classifier<-nb.classifier(train.df,config$train.classcol)
  is.valid<-length(classifier) == ncol(train.df)
  remove(train.df)
  
  if(is.valid) {
    test.df<-read.data.frame(config$test.file)
    test.df<-nb.predictor(classifier,test.df,config$test.classcol)
    write.data.frame(test.df,config$outputfile)
    remove(test.df)
  }
}
