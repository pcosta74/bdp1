source('io.R')
source('naivebayes.R')

main<-function() {
  formula=as.formula("donated ~ .")
  train="datasets/transfusion_train.csv"
  pred="datasets/transfusion_pred.csv"
  output="transfusion_pred.csv"
  
  train.data<-read.data.frame(train)
  pred.data<-read.data.frame(pred)
  out.data<-naivebayes(formula, train.data, pred.data)
  write.data.frame(out.data, output)
}

