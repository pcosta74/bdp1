source('io.R')
source('naivebayes.R')

main<-function() {
  formula<-as.formula("eval ~ .")
  train<-"datasets/cars_train.csv"
  pred<-"datasets/cars_pred.csv"
  output<-NULL #"cars_pred.csv"

  train.data<-read.data.frame(train, na.strings=c("","?"))
  pred.data<-read.data.frame(pred, na.strings=c("","?"))
  out.data<-naivebayes(formula, train.data, pred.data)
  # write.data.frame(out.data,output)
}

