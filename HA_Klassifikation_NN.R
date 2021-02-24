
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
#coffeeTable$treue = as.factor(coffeeTable$treue)

# Hilfmethode zur bewertung der modelle 
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



# daten standardisieren (nur numerische)
coffeeTable.stand = coffeeTable

## macht die haushalts nummer überhaupt sinn?
coffeeTable.stand$nummer = (coffeeTable$nummer - mean(coffeeTable$nummer)) / sd(coffeeTable$nummer)

coffeeTable.stand$dauer = (coffeeTable$dauer - mean(coffeeTable$dauer)) / sd(coffeeTable$dauer)
coffeeTable.stand$persn = (coffeeTable$persn - mean(coffeeTable$persn)) / sd(coffeeTable$persn)

prepData = function ( data){
  retData = data
  
  set.seed(1337)
  nr = dim(data)[1]
  retData = data[sample.int(nr),]
  trainRet = retData[1:65493,]
  testRet = retData[65493:130986,]
  
  return ( list("train" = trainRet, "test" = testRet))
  
}



table(coffeeTable.stand$preis)
str(coffeeTable.stand)

library(neuralnet)
hl1 = 15
hl2 = 3


workData = prepData(coffeeTable.stand)

# ist der Datensatz legit? Also relativ gleichmäßig treu und untrei vertreten?
hist(workData$train$treue)

#transform factors to binary vars
modelData <- model.matrix( 
   ~ . , 
  data = workData$train 
)
modelData = as.data.frame(modelData)
str(modelData)

modelData[1] = NULL # drop intercept col


# (treue == 2) für binary classifikation da wir fast nur kategoriale feature haben
knn_treue = neuralnet((treue == 2) ~ . ,
                      data = modelData,
                      hidden = c(hl1,hl2),
                      learningrate = 0.0001,
                      act.fct = "tanh",
                      lifesign = "full",
                      algorithm = "rprop+",
                      threshold = 0.15,
                      stepmax=1e6,
                      linear.output = FALSE
)

plot(knn_treue)

nn = neuralnet((treue == 2) ~ . , modelData, hidden = c(hl1,hl2), threshold = 0.15, lifesign = "full", linear.output = FALSE)
plot(nn)
saveRDS(nn, "nn_logistic.net")
nn_tanh = neuralnet((treue == 2) ~ . , modelData, hidden = c(hl1,hl2), act.fct = "tanh", threshold = 0.15, lifesign = "full", linear.output = FALSE)
saveRDS(nn_tanh, "nn_tanh.net")


# anwenden
modelDataTest = model.matrix( 
  ~ . , 
  data = workData$test 
)
modelDataTest = as.data.frame(modelDataTest)
modelDataTest[1] = NULL # drop intercept col


# logistic
testDataComputed = compute(nn, modelDataTest)
summary(testDataComputed)
testDataComputed.result = testDataComputed$net.result


# Prediction umwandeln in unsere treue faktoren
testDataComputed.result[which(testDataComputed.result[,1] > 0.5)] = 2
testDataComputed.result[which(testDataComputed.result[,1] <= 0.5)] = 1

#Ergebnis vergleichen mit tatsächlicher treue
conf_matrix = table(pred = testDataComputed.result, true = modelDataTest$treue)
print(conf_matrix)
kennzahlen = calcPerformance(conf_matrix)

# 64% Accuracy
# 52% False-Positive Rate
# 76% true-positive-rate

# tanh
testDataComputed = compute(nn_tanh, modelDataTest)
summary(testDataComputed)
testDataComputed.result = testDataComputed$net.result


# Prediction umwandeln in unsere treue faktoren
testDataComputed.result[which(testDataComputed.result[,1] > 0.5)] = 2
testDataComputed.result[which(testDataComputed.result[,1] <= 0.5)] = 1

#Ergebnis vergleichen mit tatsächlicher treue
conf_matrix = table(pred = testDataComputed.result, true = modelDataTest$treue)
print(conf_matrix)
kennzahlen = calcPerformance(conf_matrix)

# 62% accuracy
# 67% False-Positive-Rate
# 85% True-positive-Rate

# -> Logistic activation func vmtl besser

### Automatisierung von Parameteroptimierung

act.func = c("tanh","logistic")
hl1 = c(20,15,10)
hl2 = c(8,4,2)

## verschiedene NN trainieren
for(act_func in act.func){
  for(hl_1 in hl1){
    for(hl_2 in hl2){
      nn_tmp = neuralnet((treue == 2) ~ . ,
                         modelData,
                         hidden = c(hl_1,hl_2),
                         learningrate = 0,0001,
                         act.fct = act_func,
                         lifesign = "full",
                         algorithm = "rprop+",
                         threshold = 0.15,
                         stepmax = 50000,
                         linear.output = FALSE)
      
      # speichern damit wir das nicht 100x machen müssen
      filename = paste0("nn_auto_",act_func,"_",hl_1,"_",hl_2,".net")
      saveRDS(nn_tmp, filename)
    }
  }
}

#Auswertungen vergleichen
for(act_func in act.funct){
  for(hl_1 in hl1){
    for(hl_2 in hl2){
      filename = paste0("nn_auto_",act_func,"_",hl_1,"_",hl_2,".net")
      nn_tmp = readRDS(filename)
      
      testDataComputed = compute(nn, modelDataTest)
      summary(testDataComputed)
      testDataComputed.result = testDataComputed$net.result
      
      
      # Prediction umwandeln in unsere treue faktoren
      testDataComputed.result[which(testDataComputed.result[,1] > 0.5)] = 2
      testDataComputed.result[which(testDataComputed.result[,1] <= 0.5)] = 1
      
      #Ergebnis vergleichen mit tatsächlicher treue
      conf_matrix = table(pred = testDataComputed.result, true = modelDataTest$treue)
      kennzahlen = calcPerformance(conf_matrix)
      
      ## quotient true-pos-rate und false-pos-rate
      ## höherer wert = besser, > 1.0 heißt besser als zufällige auswahl
      print(paste0(filename, ":", kennzahlen$TPR / kennzahlen$FPR))
    }
  }
}





