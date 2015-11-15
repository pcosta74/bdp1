source('io.R')
source('naivebayes.R')

config<<-list(
  train.file="clientes_train.csv",
  train.classcol="emprestimo",
  test.file="clientes_test2.csv",
  test.classcol="emprestimo",
  outputfile="clientes_pred2.csv",
  skip.blanks=TRUE,
  log.probs=TRUE
)

run<-function() {
  train.df<-read.data.frame(config$train.file)
  classifier<-nb.classifier(train.df,config$train.classcol)
  print.train.info(train.df,classifier)
  remove(train.df)
  
  test.df<-read.data.frame(config$test.file)
  pred.df<-nb.predictor(classifier,test.df,config$test.classcol,
                        log.probs=config$log.probs)
  write.data.frame(pred.df,config$outputfile)
  remove(test.df)
  remove(pred.df)
}
