source('io.R')
source('naivebayes.R')

run<-function(formula, train.csv, test.csv, out.csv) {
  train.data<-read.data.frame(train.csv)
  test.data<-read.data.frame(test.csv)
  relation<-substr(train.csv,1,regexpr("\\.",train.csv) - 1)
  pred.data<-naivebayes(relation, formula, train.data, test.data)
  write.data.frame(pred.data,out.csv)
}

main<-function() {
  formula<-as.formula("emprestimo ~ .")
  train<-"clientes_train.csv"
  test<-"clientes_test2.csv"
  output<-"clientes_pred2.csv"
  run(formula, train, test, output)
}