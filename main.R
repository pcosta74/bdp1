source('io.R')
source('naivebayes.R')

config.train<-list(file="clientes_train.csv",classcolumn="emprestimo")
config.test<-list(file="clientes_test.csv",classcolumn="emprestimo")

train.ds<-read.dataset(config.train$file)
train.ds<-preprocess.dataset(train.ds)
classifier<-nb.classifier(config.train$classcolumn,train.ds)

test.ds<-read.dataset(config.test$file)
test.ds<-preprocess.dataset(test.ds)
prediction<-nb.predictor(classifier,config.test$classcolumn,test.ds)

write.dataset(prediction)

