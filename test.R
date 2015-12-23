source('io.R')
source('naivebayes.R')

scenario<-list(
  'cars'=list(
    formula=eval ~ .,
    train="datasets/cars_train.csv", 
    pred="datasets/cars_pred.csv",
    output="output/cars_pred.csv",
    plot="output/cars_pred.png"
  ),
  'tennis'=list(
    formula=play ~ .,
    train="datasets/tennis_train.csv", 
    pred="datasets/tennis_pred.csv",
    output="output/tennis_pred.csv",
    plot="output/tennis_pred.png",
    percent.split=1
  ),
  'clients'=list(
    formula=emprestimo ~ .,
    train="datasets/clientes_train.csv", 
    pred="datasets/clientes_pred.csv",
    output="output/clientes_pred.csv",
    plot="output/clientes_pred.png"
  ),
  'scale'=list(
    formula=scales ~ .,
    train="datasets/balance_scale_train.csv", 
    pred="datasets/balance_scale_pred.csv",
    output="output/balance_scale_pred.csv",
    plot="output/balance_scale_pred.png"
  ),
  'votes'=list(
    formula=party ~ .,
    train="datasets/house_votes_84_train.csv", 
    pred="datasets/house_votes_84_pred.csv",
    output="output/house_votes_84_pred.csv",
    plot="output/house_votes_84_pred.png"
  ),
  'lenses'=list(
    formula=contact_lenses ~ .,
    train="datasets/lenses_train.csv", 
    pred="datasets/lenses_pred.csv",
    output="output/lenses_pred.csv",
    plot="output/lenses_pred.png",
    percent.split=1
  ),
  'transfusion'=list(
    formula=donated ~ .,
    train="datasets/transfusion_train.csv", 
    pred="datasets/transfusion_pred.csv",
    output="output/transfusion_pred.csv",
    plot="output/transfusion_pred.png"
  ),
  'poker'=list(
    formula=Poker_Hand ~ .,
    train="datasets/poker_hand_train.csv", 
    pred="datasets/poker_hand_pred.csv",
    output="output/poker_hand_pred.csv",
    plot="output/poker_hand_pred.png"
  ),
  'iris'=list(
    formula=Species ~ .,
    train="datasets/iris_train.csv", 
    pred="datasets/iris_pred.csv",
    output="output/iris_pred.csv",
    plot="output/iris_pred.png"
  ),
  'titanic'=list(
    formula=Class ~ .,
    train="datasets/titanic_train.csv", 
    pred="datasets/titanic_pred.csv",
    output="output/titanic_pred.csv",
    plot="output/titanic_pred.png"
  )
)

test<-function(formula,train, pred=NULL, output=NULL, plot=NULL, percent.split=0.7) {
  train.data<-read.data.frame(train, na.strings=c("","?"))
  pred.data<-read.data.frame(pred, na.strings=c("","?"))
  out.data<-naivebayes(formula, train.data, pred.data, percent.split)
  write.data.frame(out.data, output)
  if(!is.null(plot)) {
    dev.copy(png, plot, width=600, height=600)
    dev.off()
  }
}

run<-function() {
  trycatch({
    con1<-file("output/run.log","w")
    sink(con1, append=TRUE)

    con2<-file("output/msg.log","w")
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
  },
  error = function(err) { stop(err) },
  warning = function(war) { warning(war) },
  finally = {
    sink() 
    sink(type="message")
  })
}

