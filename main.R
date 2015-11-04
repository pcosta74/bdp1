source('io.R')
source('naivebayes.R')

config.train<<-list(file="clientes_train.csv",classcolumn="emprestimo")
config.test<<-list(file="clientes_test.csv",classcolumn="emprestimo",outputfile="clientes_pred.csv")

run<-function() {
  train.ds<-read.dataset(config.train$file)
  train.ds<-preprocess.dataset(train.ds)

  classifier<-nb.classifier(config.train$classcolumn,train.ds)
  is.valid<-length(classifier) == ncol(train.ds)
  remove(train.ds)
  
  #t<-as.table(t(classifier[[config.train$classcolumn]]))
  #rownames(t)<-c("")
  #cat("A priori probabilities:\n")
  #print(t)
  
  if(is.valid) {
    test.ds<-read.dataset(config.test$file)
    test.ds<-preprocess.dataset(test.ds)
    
    test.ds<-nb.predictor(classifier,config.test$classcolumn,test.ds)
    write.dataset(test.ds,config.test$outputfile)
    remove(test.ds)
  }
}
