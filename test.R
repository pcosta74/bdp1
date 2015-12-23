source('io.R')
source('naivebayes.R')

scenario<-list(
  'cars'=list(
    formula=eval ~ .,
    train="datasets/cars_train.csv", 
    pred="datasets/cars_pred.csv",
    output="pred/cars_pred.csv",
    plot="plot/cars_test.png"
  ),
  'tennis'=list(
    formula=play ~ .,
    train="datasets/tennis_train.csv", 
    pred="datasets/tennis_pred.csv",
    output="pred/tennis_pred.csv",
    plot="plot/tennis_test.png"
  ),
  'clients'=list(
    formula=emprestimo ~ .,
    train="datasets/clientes_train.csv", 
    pred="datasets/clientes_pred.csv",
    output="pred/clientes_pred.csv",
    plot="plot/clientes_test.png"
  ),
  'scale'=list(
    formula=scales ~ .,
    train="datasets/balance_scale_train.csv", 
    pred="datasets/balance_scale_pred.csv",
    output="pred/balance_scale_pred.csv",
    plot="plot/balance_scale_test.png"
  ),
  'votes'=list(
    formula=party ~ .,
    train="datasets/house_votes_84_train.csv", 
    pred="datasets/house_votes_84_pred.csv",
    output="pred/house_votes_84_pred.csv",
    plot="plot/house_votes_84_test.png"
  ),
  'lenses'=list(
    formula=contact_lenses ~ .,
    train="datasets/lenses_train.csv", 
    pred="datasets/lenses_pred.csv",
    output="pred/lenses_pred.csv",
    plot="plot/lenses_test.png"
  ),
  'transfusion'=list(
    formula=donated ~ .,
    train="datasets/transfusion_train.csv", 
    pred="datasets/transfusion_pred.csv",
    output="pred/transfusion_pred.csv",
    plot="plot/transfusion_test.png"
  ),
  'poker'=list(
    formula=Poker_Hand ~ .,
    train="datasets/poker_hand_train.csv", 
    pred="datasets/poker_hand_pred.csv",
    output="pred/poker_hand_pred.csv",
    plot="plot/poker_hand_test.png"
  ),
  'iris'=list(
    formula=Species ~ .,
    train="datasets/iris_train.csv", 
    pred="datasets/iris_pred.csv",
    output="pred/iris_pred.csv",
    plot="plot/iris_test.png"
  ),
  'titanic'=list(
    formula=Survived ~ .,
    train="datasets/titanic_train.csv", 
    pred="datasets/titanic_pred.csv",
    output="pred/titanic_pred.csv",
    plot="plot/titanic_test.png"
  )
)

test<-function(formula,train, pred=NULL, output=NULL, plot=NULL, percent.split=0.7) {
  train.data<-read.data.frame(train, na.strings=c("","?"))
  pred.data<-read.data.frame(pred, na.strings=c("","?"))
  out.data<-naivebayes(formula, train.data, pred.data, percent.split)
  write.data.frame(out.data, output)
  write.plot(plot)
}

run<-function() {
  basepath<-getwd()
  for(k in 1:10) {
    message("Run #",k)
    outpath<-sprintf("output/run%02d",k)
    dir.create(outpath)
    setwd(outpath)
    
    con1<-file("run.log","w")
    sink(con1, append=TRUE)
    
    con2<-file("msg.log","w")
    sink(con2, append=TRUE, type="message")
    
    for(name in names(scenario)) {
      message("=== BEGIN ",Sys.time()," ===",appendLF=TRUE)
      message("Scenario: ",name,appendLF=TRUE)
      lst<-lapply(scenario[[name]],function(x) {
        ifelse(is.character(x),x,deparse(x))
      })
      message(sprintf("   %-8s: %s\n",names(lst),lst)) 
      message("===",appendLF=TRUE)
      lst<-system.time(do.call(test,scenario[[name]]))
      message("===",appendLF=TRUE)
      message("Execution:", appendLF=TRUE)
      message(sprintf("   %-10s: %s\n",names(lst),lst))
      message("=== END ",Sys.time()," ===",appendLF=TRUE)
    }
    
    sink()
    sink("message")
    
    setwd(basepath)
  }
}
