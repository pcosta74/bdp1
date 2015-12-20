source('io.R')
source('naivebayes.R')

scenario<-list(
  'poker'=list(
    formula=as.formula("Poker_Hand ~ ."),
    train="datasets/poker_hand_train.csv", 
    pred="datasets/poker_hand_pred.csv",
    output="poker_hand_pred.csv"
  ),
  'cars'=list(
    formula=as.formula("eval ~ ."),
    train="datasets/cars_train.csv", 
    pred="datasets/cars_pred.csv",
    output="cars_pred.csv"
  ),
  'tennis'=list(
    formula=as.formula("play ~ ."),
    train="datasets/tennis_train.csv", 
    pred="datasets/tennis_pred.csv",
    output="tennis_pred.csv",
    percent.split=1
  ),
  'clients'=list(
    formula=as.formula("emprestimo ~ ."),
    train="datasets/clientes_train.csv", 
    pred="datasets/clientes_pred.csv",
    output="clientes_pred.csv"
  ),
  'scale'=list(
    formula=as.formula("scales ~ ."),
    train="datasets/balance_scale_train.csv", 
    pred="datasets/balance_scale_pred.csv",
    output="balance_scale_pred.csv"
  ),
  'votes'=list(
    formula=as.formula("party ~ ."),
    train="datasets/house_votes_84_train.csv", 
    pred="datasets/house_votes_84_pred.csv",
    output="house_votes_84_pred.csv"
  ),
  'lenses'=list(
    formula=as.formula("contact_lenses ~ ."),
    train="datasets/lenses_train.csv", 
    pred="datasets/lenses_pred.csv",
    output="lenses_pred.csv",
    percent.split=1
  ),
  'transfusion'=list(
    formula=as.formula("donated ~ ."),
    train="datasets/transfusion_train.csv", 
    pred="datasets/transfusion_pred.csv",
    output="transfusion_pred.csv"
  )
)

run<-function(formula,train, pred=NULL, output=NULL, percent.split=0.7) {
  train.data<-read.data.frame(train, na.strings=c("","?"))
  pred.data<-read.data.frame(pred, na.strings=c("","?"))
  out.data<-naivebayes(formula, train.data, pred.data, percent.split)
  write.data.frame(out.data, output)
}

main<-function() {
  do.call(run,scenario$poker)
}

