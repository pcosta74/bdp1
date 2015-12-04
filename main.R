source('io.R')
source('naivebayes.R')

run<-function(formula, train.csv, test.csv, out.csv) {
  train.data<-read.data.frame(train.csv, blank.strings=c("","?"))
  test.data<-read.data.frame(test.csv, blank.strings=c("","?"))
  pred.data<-naivebayes(formula, train.data, test.data)
  write.data.frame(pred.data,out.csv)
}

main<-function() {
  formula<-as.formula("emprestimo ~ .")
  train<-"clientes_train.csv"
  pred<-"clientes_test.csv"
  output<-"clientes_pred.csv"
  run(formula, train, test, output)
}
