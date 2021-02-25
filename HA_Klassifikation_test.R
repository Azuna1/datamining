
# alle geladenen objekte entfernen
rm(list = ls()) 

# Daten importieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

coffeeTable = read.delim("kaffee.asc",sep=" ")

# daten aufbereiten ####
#datentyp der einzelnen spalten korrigieren
coffeeTable$menge = as.factor(coffeeTable$menge)
coffeeTable$preis = as.factor(coffeeTable$preis)
coffeeTable$marke = as.factor(coffeeTable$marke)
coffeeTable$alter = as.factor(coffeeTable$alter)
coffeeTable$klasse = as.factor(coffeeTable$klasse)
coffeeTable$einkm = as.factor(coffeeTable$einkm)
coffeeTable$prbew = as.factor(coffeeTable$prbew)
coffeeTable$bildg = as.factor(coffeeTable$bildg)
coffeeTable$treue = as.factor(coffeeTable$treue)

# daten standardisieren (nur numerische)
coffeeTable.stand = coffeeTable

## macht die haushalts nummer überhaupt sinn?
coffeeTable.stand$nummer = (coffeeTable$nummer - mean(coffeeTable$nummer)) / sd(coffeeTable$nummer)

coffeeTable.stand$dauer = (coffeeTable$dauer - mean(coffeeTable$dauer)) / sd(coffeeTable$dauer)
coffeeTable.stand$persn = (coffeeTable$persn - mean(coffeeTable$persn)) / sd(coffeeTable$persn)



# Hilfmethode zur bewertung der modelle ####
calcPerformance = function(data){
  a = data[1,1]
  b = data[1,2]
  c = data[2,1]
  d = data[2,2]
  
  acc = (a+d)/(a+b+c+d)
  T = a+d
  F = b+c
  R = a+c
  I = b+d
  P = a+b
  N = d+c
  
  in_acc = F / (a+b+c+d)
  
  TPR = a / R
  TNR = d / I
  FPR = b / I
  FNR = c / R
  TPR = a / R
  
  precision = a / P
  ret = data.frame(T, F, R, I, P, N, acc, in_acc, TPR, TNR, FPR, FNR, precision)
  return (ret)
}


library(C50)
library(RWeka)



# m5p #

# funktioniert nicht mit kategorialen werten, daher dummyvar encoding
runM5P = function(train_, test_){
  train_ <- model.matrix( 
    ~ . , 
    data = train_
  )
  train_ = as.data.frame(train_)
  train_[1] = NULL
  colnames(train_)[9] = "treue"
  
  
  test_ <- model.matrix( 
    ~ . , 
    data = train_
  )
  
  
  test_ = as.data.frame(test_)
  test_[1] = NULL
  colnames(test_)[9] = "treue"
  
  model.m5p = M5P(treue ~., data=train_)
  prediction.m5p = predict(model.m5p, test_, type="class")
  
  #transformieren zu treu / untreu
  prediction.m5p[which(prediction.m5p > 0.5)] = 2
  prediction.m5p[which(prediction.m5p <= 0.5)] = 1
  
  confusion.matrix.m5p = table(prediction.m5p,test_$treue)
  return(confusion.matrix.m5p)
}

# c4.5 #
runC45 = function(train_, test_)
{
  model.c45 = LMT(treue ~., data=train_)
  prediction.c45 = predict(model.c45, test_, type="class")
  
  confusion.matrix.c45 = table(prediction.c45,test_$treue)
  colnames(confusion.matrix.c45) = paste("true", rownames(confusion.matrix.c45), sep=":")
  rownames(confusion.matrix.c45) = paste("pred", colnames(confusion.matrix.c45), sep=":")
  
  return(list("model" = model.c45, "pred" = prediction.c45,"confusion" = confusion.matrix.c45, "kennzahlen" = calcPerformance(confusion.matrix.c45)))
}
# c5.0 #

runC50 = function(train_, test_)
{

  model.c50 = C5.0(treue ~., data=train_)
  prediction.c50 = predict(model.c50, test_, type="class")
  
  confusion.matrix.c50 = table(prediction.c50,test_$treue)
  colnames(confusion.matrix.c50) = paste("true", rownames(confusion.matrix.c50), sep=":")
  rownames(confusion.matrix.c50) = paste("pred", colnames(confusion.matrix.c50), sep=":")
  
  return(list("model" = model.c50, "pred" = prediction.c50,"confusion" = confusion.matrix.c50, "kennzahlen" = calcPerformance(confusion.matrix.c50)))
  

}

# J48 #

runJ48 = function(train_, test_)
{
  model.j48 = J48(treue ~., data=train_)
  prediction.j48 = predict(model.j48, test_,type="class")
  confusion.matrix.j48 = table(prediction.j48,test_$treue)
  colnames(confusion.matrix.j48) = paste("true", rownames(confusion.matrix.j48), sep=":")
  rownames(confusion.matrix.j48) = paste("pred", colnames(confusion.matrix.j48), sep=":")

  return (calcPerformance(confusion.matrix.j48))
}

prepData = function ( data){
  retData = data
  
  set.seed(1337)
  nr = dim(data)[1]
  retData = data[sample.int(nr),]
  trainRet = retData[1:65493,]
  testRet = retData[65493:130986,]
  
  return ( list("train" = trainRet, "test" = testRet))
  
}



# testdaten festlegen  ####


# attribute ausklammern
#selectedData$nummer = NULL

# nur diese attribute beachten
#selectedData = selectedData[,c(2,8,10,12)]


## vergleich der modelle anhand unserer ausgewählten attribute ####
selectedData = coffeeTable.stand
selectedData = selectedData[,c(4,12)] # Treue anhand der Marke
workData = prepData(selectedData)

runC45(workData$train, workData$test)
runC50(workData$train, workData$test)
runJ48(workData$train, workData$test)

selectedData = coffeeTable.stand
selectedData = selectedData[,c(2,8,10,12)] #treue anhand von preis, einkommen, preisbewusstsein
workData = prepData(selectedData)

a = runC45(workData$train, workData$test)
a = runC50(workData$train, workData$test)
runJ48(workData$train, workData$test)

a$model
a$pred
a$confusion
plot(a$model)

# enorm schlechte TPR und FPR sowie nur eine acc von ~55% bei allen verfahren

