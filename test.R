source('io.R')
source('naivebayes.R')

scenario<-list(
  'cars'=list(
    formula=eval ~ .,
    train="datasets/cars_train.csv", 
    pred="datasets/cars_pred.csv",
    output="output/cars_pred.csv"
  ),
  'tennis'=list(
    formula=play ~ .,
    train="datasets/tennis_train.csv", 
    pred="datasets/tennis_pred.csv",
    output="output/tennis_pred.csv",
    percent.split=1
  ),
  'clients'=list(
    formula=emprestimo ~ .,
    train="datasets/clientes_train.csv", 
    pred="datasets/clientes_pred.csv",
    output="output/clientes_pred.csv"
  ),
  'scale'=list(
    formula=scales ~ .,
    train="datasets/balance_scale_train.csv", 
    pred="datasets/balance_scale_pred.csv",
    output="output/balance_scale_pred.csv"
  ),
  'votes'=list(
    formula=party ~ .,
    train="datasets/house_votes_84_train.csv", 
    pred="datasets/house_votes_84_pred.csv",
    output="output/house_votes_84_pred.csv"
  ),
  'lenses'=list(
    formula=contact_lenses ~ .,
    train="datasets/lenses_train.csv", 
    pred="datasets/lenses_pred.csv",
    output="output/lenses_pred.csv",
    percent.split=1
  ),
  'transfusion'=list(
    formula=donated ~ .,
    train="datasets/transfusion_train.csv", 
    pred="datasets/transfusion_pred.csv",
    output="output/transfusion_pred.csv"
  ) #,
  #'poker'=list(
  #  formula=Poker_Hand ~ .,
  #  train="datasets/poker_hand_train.csv", 
  #  pred="datasets/poker_hand_pred.csv",
  #  output="output/poker_hand_pred.csv"
  #)
)

test<-function(formula,train, pred=NULL, output=NULL, percent.split=0.7) {
  train.data<-read.data.frame(train, na.strings=c("","?"))
  pred.data<-read.data.frame(pred, na.strings=c("","?"))
  out.data<-naivebayes(formula, train.data, pred.data, percent.split)
  write.data.frame(out.data, output)
}

run<-function() {
  for(name in names(scenario)) {
    message("=== BEGIN ===",appendLF=TRUE)
    do.call(test,scenario[[name]])
    message("=== END ===",appendLF=TRUE)
  }
}

