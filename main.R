source('io.R')
source('naivebayes.R')

main<-function() {
  formula<-as.formula("play ~ .")
  train<-"datasets/tennis_train.csv"
  pred<-"datasets/tennis_pred.csv"
  output<-NULL #"cars_pred.csv"

  train.data<-read.data.frame(train, blank.strings=c("","?"))
  pred.data<-read.data.frame(pred, blank.strings=c("","?"))
  out.data<-naivebayes(formula, train.data, pred.data)
  write.data.frame(out.data,output)
}

