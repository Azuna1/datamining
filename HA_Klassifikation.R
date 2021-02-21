
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


# testdaten festlegen  ####

# attribute ausklammern
selectedData = coffeeTable.stand
selectedData$nummer = NULL

# nur diese attribute beachten
selectedData = selectedData[,c(2,8,10,12)]



train = selectedData[1:65493,]
test = selectedData[65493:130986,]





# m5p ####

library(RWeka)

model.m5p = M5P(treue ~., data=train)
prediction.m5p = predict(model.m5p, test,type="class")

#transformieren zu treu / untreu
prediction.m5p[which(prediction.m5p <= 0.5)] = 0
prediction.m5p[which(prediction.m5p > 0.5)] = 0

confusion.matrix.m5p = table(prediction.m5p,test$treue)
colnames(confusion.matrix.m5p) = paste("true", rownames(confusion.matrix.m5p), sep=":")
rownames(confusion.matrix.m5p) = paste("pred", colnames(confusion.matrix.m5p), sep=":")

kennzahlen.m5p = calcPerformance(confusion.matrix.m5p)


# c4.5 ####
model.c45 = LMT(treue ~., data=train)
prediction.c45 = predict(model.c45, test,type="class")

confusion.matrix.c45 = table(prediction.c45,test$treue)
colnames(confusion.matrix.c45) = paste("true", rownames(confusion.matrix.c45), sep=":")
rownames(confusion.matrix.c45) = paste("pred", colnames(confusion.matrix.c45), sep=":")

kennzahlen.c45 = calcPerformance(confusion.matrix.c45)

# c5.0 ####
library(C50)

trainF = train
trainF$treue = as.factor(train$treue)
testF = test
testF$treue = as.factor(test$treue)

model.c50 = C5.0(treue ~., data=trainF)
prediction.c50 = predict(model.c50, testF, type="class")

confusion.matrix.c50 = table(prediction.c50,testF$treue)
colnames(confusion.matrix.c50) = paste("true", rownames(confusion.matrix.c50), sep=":")
rownames(confusion.matrix.c50) = paste("pred", colnames(confusion.matrix.c50), sep=":")

kennzahlen.c50 = calcPerformance(confusion.matrix.c50)

# J48 ####
model.j48 = J48(treue ~., data=train)
prediction.j48 = predict(model.j48, test,type="class")
confusion.matrix.j48 = table(prediction.j48,test$treue)
colnames(confusion.matrix.j48) = paste("true", rownames(confusion.matrix.j48), sep=":")
rownames(confusion.matrix.j48) = paste("pred", colnames(confusion.matrix.j48), sep=":")

kennzahlen.j48 = calcPerformance(confusion.matrix.j48)

## vergleich der modelle anhand unserer ausgewählten attribute ####

kennzahlen.m5p
kennzahlen.c45
kennzahlen.c50
kennzahlen.j48
