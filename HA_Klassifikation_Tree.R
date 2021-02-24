
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

# binäre form
coffeeTable$treue[which(coffeeTable$treue == 2)] = 0
coffeeTable$treue = as.factor(coffeeTable$treue)


# daten standardisieren (nur numerische)
coffeeTable.stand = coffeeTable

## macht die haushalts nummer überhaupt sinn?
coffeeTable.stand$nummer = (coffeeTable$nummer - mean(coffeeTable$nummer)) / sd(coffeeTable$nummer)

coffeeTable.stand$dauer = (coffeeTable$dauer - mean(coffeeTable$dauer)) / sd(coffeeTable$dauer)
coffeeTable.stand$persn = (coffeeTable$persn - mean(coffeeTable$persn)) / sd(coffeeTable$persn)


require(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# 1  menge
# 2  preis
# 3  nummer
# 4  marke
# 5  dauer
# 6  alter
# 7  klasse
# 8  einkm
# 9  persn
# 10 prbew
# 11 bildg
# 12 treue

# Gesamten Datensatz analysieren
selectedData = coffeeTable.stand
workData = prepData(selectedData)
rfit = rpart(treue ~., data = workData$train, method = "class", cp=0.00001)
printcp(rfit)
# geringste xerror wert = 0.81960 -> viel zu hoch

prediction = predict(rfit, workData$test, type="class")
confusion = table(prediction, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
zahlen = calcPerformance(confusion)
zahlen
#   T     F     R     I    P     N      acc       in_acc    TPR       TNR        FPR        FNR   precision
# 42370 23124 27991 37503 26195 39299 0.6469295 0.3530705 0.5548569 0.7156494 0.2843506 0.4451431 0.5928994 

print(zahlen$TPR/zahlen$FPR)
# 1.951313 -> Wert > 1 bedeuted besser als raten!


# da wir uns eh unsicher sind bei der Haushaltsnummer, was passiert wenn wir die ausklammern?
selectedData = coffeeTable.stand
selectedData$nummer = NULL
workData = prepData(selectedData)
rfit = rpart(treue ~., data = workData$train, method = "class", cp=0.00001)
printcp(rfit)
# geringste xerror wert = 0.86159 -> viel zu hoch, sogar noch höher!

prediction = predict(rfit, workData$test, type="class")
confusion = table(prediction, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
zahlen = calcPerformance(confusion)
zahlen
#   T     F     R     I    P     N      acc       in_acc    TPR       TNR        FPR        FNR   precision
# 40695 24799 27991 37503 25260 40234 0.6213546 0.3786454 0.5082348 0.7057835 0.2942165 0.4917652 0.5631829

print(zahlen$TPR/zahlen$FPR)
# 1.727418 -> Wert > 1 bedeuted besser als raten!


# menge, preis, marke
selectedData = coffeeTable.stand[,c(1,2,4,12)]
workData = prepData(selectedData)
rfit = rpart(treue ~., data = workData$train, method = "class", cp=0.00001)
printcp(rfit)
# geringste xerror wert = 0.94835 -> viel zu hoch, sogar noch höher!

prediction = predict(rfit, workData$test, type="class")
confusion = table(prediction, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
zahlen = calcPerformance(confusion)
zahlen

#   T     F     R     I    P     N      acc       in_acc    TPR       TNR        FPR        FNR   precision
# 38772 26722 27991 37503 12155 53339 0.5919932 0.4080068 0.2397914 0.8548649 0.1451351 0.7602086 0.5522007 

print(zahlen$TPR/zahlen$FPR)
# 1.652195 -> Wert > 1 bedeuted besser als raten!



# preis, einkm, preisbw, bildg
selectedData = coffeeTable.stand[,c(2,8,10,11,12)]
workData = prepData(selectedData)
rfit = rpart(treue ~., data = workData$train, method = "class", minsplit = 2, minbucket = 1, cp=0.00001)
printcp(rfit)
# geringste xerror wert = 0.99896 -> viel zu hoch, sogar noch höher!

prediction = predict(rfit, workData$test, type="class")
confusion = table(prediction, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
zahlen = calcPerformance(confusion)
zahlen

#   T     F     R     I    P     N      acc       in_acc    TPR       TNR        FPR        FNR   precision
# 37533 27961 27991 37503 3268 62226 0.5730754 0.4269246 0.05891179 0.9568301 0.04316988 0.9410882   0.50459 

print(zahlen$TPR/zahlen$FPR)
# 1.36465 -> Wert > 1 bedeuted besser als raten!


# marke, klasse, einkm
selectedData = coffeeTable.stand[,c(2,7,8,12)]
workData = prepData(selectedData)
rfit = rpart(treue ~., data = workData$train, method = "class", minsplit = 2, minbucket = 1, cp=0.00001)
printcp(rfit)
# geringste xerror wert = 0.99900 -> viel zu hoch

# fancyRpartPlot(rfit, caption=NULL) # Zu komplexe Trees für RStudio plot
# summary(rfit)

prediction = predict(rfit, workData$test, type="class")
confusion = table(prediction, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
zahlen = calcPerformance(confusion)
zahlen 

#   T     F     R     I    P     N      acc       in_acc    TPR       TNR        FPR        FNR   precision
# 7660  27834 27991 37503 1675 63819 0.5750145 0.4249855 0.0327248 0.9797616 0.02023838 0.9672752 1 0.5468657 

print(zahlen$TPR/zahlen$FPR)
# 1.616968 -> Wert > 1 bedeuted besser als raten!

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
