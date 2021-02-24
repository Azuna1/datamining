
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

# binäre form / boolean interpretation
coffeeTable$treue[which(coffeeTable$treue == 2)] = 0
coffeeTable$treue = as.factor(coffeeTable$treue)


# daten standardisieren (nur numerische)
coffeeTable.stand = coffeeTable

## macht die haushaltsnummer überhaupt sinn?
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


prepData = function ( data){
  retData = data
  
  set.seed(1337)
  nr = dim(data)[1]
  retData = data[sample.int(nr),]
  trainRet = retData[1:65493,]
  testRet = retData[65493:130986,]
  
  return ( list("train" = trainRet, "test" = testRet))
  
}



# Encoden der nominalen daten nach one-hot #### 

oh_data = as.data.frame(model.matrix( ~ . , data = coffeeTable.stand))
oh_data[1] = NULL
colnames(oh_data)[32] = "treue"

# Aufteilen des Datensatzes zu train und test set
workData = prepData(oh_data)

library(class)  

# K wert ermitteln, guter startwert ist wurzel der anzahl der datensätze, hier 255.916
sqrt(NROW(workData$train))

knn.255 = knn(train=workData$train, test=workData$test, cl=workData$train$treue, k=255)
acc.255 = 100*sum(workData$test$treue == knn.255) /NROW(workData$test$treue)
knn.256 = knn(train=workData$train, test=workData$test, cl=workData$train$treue, k=256)
acc.256 = 100*sum(workData$test$treue == knn.256) /NROW(workData$test$treue)

# accuracy
#255 = 98.85486
#256 = 98.87013

# actual values
table(knn.255, workData$test$treue)
# knn.255     0     1
# 0       27334    93
# 1         657 37410

table(knn.256, workData$test$treue)
# knn.255     0     1
# 0       27335    94
# 1         656 37409

#performance
zahlen = calcPerformance(table(knn.255, workData$test$treue))
print(zahlen$TPR/zahlen$FPR)
# 393.7929


zahlen = calcPerformance(table(knn.256, workData$test$treue))
print(zahlen$TPR/zahlen$FPR)
# 389.7604

# Die werte sind enorm gut!

