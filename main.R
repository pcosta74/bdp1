source('io.R')
source('naivebayes.R')

main<-function() {
  formula<-as.formula("scales ~ .")
  train<-"datasets/balance_scale_train.csv"
  pred<-"datasets/balance_scale_pred.csv"
  output<-"" # "clientes_pred.csv"

  train.data<-read.data.frame(train, blank.strings=c("","?"))
  pred.data<-read.data.frame(pred, blank.strings=c("","?"))
  out.data<-naivebayes(formula, train.data, pred.data)
  write.data.frame(out.data,output)
}
