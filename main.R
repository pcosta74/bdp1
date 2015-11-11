source('io.R')
source('naivebayes.R')

config<<-list(
  train.file="clientes_train.csv",
  train.classcolumn="emprestimo",
  test.file="clientes_test2.csv",
  test.classcolumn="emprestimo",
  outputfile="clientes_pred2.csv"
)

run<-function() {
  train.ds<-read.dataset(config$train.file)
  classifier<-nb.classifier(config$train.classcolumn,train.ds)
  is.valid<-length(classifier) == ncol(train.ds)
  remove(train.ds)
  
  if(is.valid) {
    test.ds<-read.dataset(config$test.file)
    test.ds<-nb.predictor(classifier,config$test.classcolumn,test.ds)
    write.dataset(test.ds,config$outputfile)
    remove(test.ds)
  }
}
