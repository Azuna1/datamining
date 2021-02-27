# Daten importieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

coffeeTable = read.delim("kaffee.asc",sep=" ")



### Begutachten der Daten ####

# daten aufbereiten
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

coffeeTable.stand$nummer = (coffeeTable$nummer - mean(coffeeTable$nummer)) / sd(coffeeTable$nummer)
coffeeTable.stand$dauer = (coffeeTable$dauer - mean(coffeeTable$dauer)) / sd(coffeeTable$dauer)
coffeeTable.stand$persn = (coffeeTable$persn - mean(coffeeTable$persn)) / sd(coffeeTable$persn)





### Analyse ####

# Hilfmethode zur bewertung der modelle ####

# Berechnet Kennzahlen anhand derer wir verschiedene Analysen vergleichen können
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

# Bereitet unser Dataframe vor, als Rückgabewert erhalten wir eine Liste mit Liste$train und Liste$test
prepData = function ( data){
  retData = data
  
  set.seed(1337)
  nr = dim(data)[1]
  retData = data[sample.int(nr),]
  trainRet = retData[1:65493,]
  testRet = retData[65493:130986,]
  
  return ( list("train" = trainRet, "test" = testRet))
  
}



#### Klassifikation - Entscheidungsbaum ####

library(rpart)
library(rpart.plot)
library(rattle)

# Gesamten Datensatz analysieren
selectedData = coffeeTable.stand

# Vorbereitung unserer train und test Daten
workData = prepData(selectedData)

# Modell erstellen anhand der train Daten, Als Methode wird "class" für eine "Klassifizierung" gewählt welche das Gini Impurity Verfahren nutzt
# Der cp Parameter gibt hier die Komplexität an, nach welcher Entschieden wird, ob es einen neuen Split im Entscheidungsbaum gibt.
rfit = rpart(treue ~., data = workData$train, method = "class", cp=0.00001)

# Gibt eine Übersicht der Komplexitäten aus, Zur optimierung unseres Entscheidungsbaums sollte hier der geringste xerror Wert gewählt werden,
# und anschließend ein neues Modell mit angepasstem CP Parameter erstellt werden
printcp(rfit)

# Visualisierung des Baums
# fancyRpartPlot(rfit, caption=NULL) #Dieser Baum hat über >3000 Splits und ist zu komplex zum plotten in Rstudio

# Auswertung unseres Modells
# Vorhersage der Test Daten anhand unseres Modells der Train Daten
prediction = predict(rfit, workData$test, type="class")

# Vergleich der Test daten mit der tatsächlichen Treue
confusion = table(prediction, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
print(confusion)

# Berechnung der Kennzahlen anhand derer wir verschiedene Modelle Bewerten können
zahlen = calcPerformance(confusion)
print(zahlen)

# Quotient aus TPR / FPR
print(zahlen$TPR/zahlen$FPR)

# Vergleich zum vorherigen Beispiel mit nur 3 Merkmalen

# marke, klasse, einkm
selectedData = coffeeTable.stand[,c(2,7,8,12)]
workData = prepData(selectedData)
rfit = rpart(treue ~., data = workData$train, method = "class", minsplit = 2, minbucket = 1, cp=0.00001)
printcp(rfit)

fancyRpartPlot(rfit, caption=NULL) 

prediction = predict(rfit, workData$test, type="class")
confusion = table(prediction, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
print(confusion)

zahlen = calcPerformance(confusion)
print(zahlen)

# Quotient aus TPR / FPR
print(zahlen$TPR/zahlen$FPR)


#### Klassifikation Anhand von K-Nearest Neighbors ####

library(class)

# Für das KNN Verfahren müssen alle nominalen Merkmale zunächst "One-Hot" transformiert werden. Dabei handelt es sich um eine Binäre kodierung.
oh_data = as.data.frame(model.matrix( ~ . , data = coffeeTable.stand))
oh_data

# Die Intercept Spalte müssen wir entfernen
oh_data[1] = NULL
# Und die Treue Spalte benennen wir wieder richtig
colnames(oh_data)[32] = "treue"

# Anschließend wird der Datensatz wieder ein ein Train und ein Test set aufgeteilt
workData = prepData(oh_data)

# Des weiteren benötigt KNN einen vorgegebenen wert K für die Anzahl an zu betrachtenden Nachbarn.
# Als guter Startwerd wird hier die Wurzel der Anzahl der Einträge im Datensatz
# hier 255.916
sqrt(NROW(workData$train))


#Testweise erstellen wir 2 Modelle mit K = 255 und K = 256

knn.255 = knn(train=workData$train, test=workData$test, cl=workData$train$treue, k=255)
knn.256 = knn(train=workData$train, test=workData$test, cl=workData$train$treue, k=256)

# Vergleichen wieder die Konfusionsmatrix

confusion = table(knn.255, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
confusion

confusion = table(knn.256, workData$test$treue)
colnames(confusion) = paste("true", rownames(confusion), sep=":")
rownames(confusion) = paste("pred", colnames(confusion), sep=":")
confusion


# Und auch die Kennzahlen
zahlen = calcPerformance(table(knn.255, workData$test$treue))
zahlen
# Quotient aus TPR / FPR
print(zahlen$TPR/zahlen$FPR)


zahlen = calcPerformance(table(knn.256, workData$test$treue))
zahlen
# Quotient aus TPR / FPR
print(zahlen$TPR/zahlen$FPR)




#### Clusteranalyse ####

# Da es sich bei dem hier behandelten Datensatz um hauptsächlich nominale Merkmale handelt müssen wir ein Verfahren wählen,
# welches die Distanz der einzelnen Einträge anhand von einem Ähnlichkeitsverfahren bestimmt.
# Leider benötigen diese Verfahren eine enorme Menge an Arbeitsspeicher.

### partitionierend ###

# Kproto basiert auf k-means, ermöglicht aber auch den vergleich von nominalen Merkmalen mithilfe des Simple-Matching verfahren

library(clustMixType)

# Auch hier wird ein Parameter k benötigt, der angibt wieviele Cluster gebildet werden sollen.
# Um uns ein Bild über den einfluss von K zu machen probieren wir k[1:10] aus und erstellen einen ScreePlot

for(i in 1:10){
  kpres = kproto(x = coffeeTable.stand, k = i, nstart = 5)
  Es[i] = kpres$tot.withinss
}

plot(1:10, Es, type = "b", ylab = "Objective Function", xlab = "# Clusters",
     main = "Scree Plot") 


# Da der Verlauf zwischen 6 und 10 Clustern nahezu lienar wird wählen wir k = 8, viel mehr Cluster sind für uns vermutlich ohnehin nicht von relevanz
kpres = kproto(x = coffeeTable.stand, k = 8)

# Mit summary() können wir uns de cluster und ihre Distanzen zueinander ausgeben lassen
summary(kpres)

# Mit clprofiles() können wir uns die verschiedenen Cluster bezogen auf ein Entsprechendes Merkmal visualisieren
clprofiles(kpres, coffeeTable.stand)





























