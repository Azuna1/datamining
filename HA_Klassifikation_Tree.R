
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

# alles außer nummer und dauer
selectedData = coffeeTable.stand
selectedData$nummer = NULL
selectedData$dauer = NULL

# marke,bildg,klasse,preis
selectedData = coffeeTable.stand[,c(4,11,7,2,12)]


workData = prepData(selectedData)

rfit = rpart(treue ~., data = workData$train, method = "class", cp = 0.0001)
plot(rfit, uniform=TRUE)
text(rfit,cex=0,7)
summary(rfit)

prediction = predict(rfit, workData$test, type="class")
confusion = table(prediction, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
calcPerformance(confusion)

# Ergebnis: Marke>dauer>

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
