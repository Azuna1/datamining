### Dem Datensatz liegen die Kaffeekäufe von 2752 Haushalten zu Grunde,
### die kontinuierlich in der Zeit vom 1.1.1988 bis 31.12.1990 am Haushaltspanel teilnahmen.
### Insgesamt wurden in diesem Zeitraum 130986 Kaffeekäufe registriert.

### TODO
# Datensatz gruppieren, Haushalte verändern sich über die Zeit
# 

### Anmerkung
# Der Datensatz enthalt garkeine 2752 Haushalte
# Der Datensatz wurde "kontinuierlich" erstellt, 
# jedoch nicht erkennbar in welcher folge, da nach haushalt sortiert ist
# und kein erhebungs zeitpunkt bekannt
# Sequenzanalyse per haushalt?!?!

library(cluster)
library(tidyverse)

# to remove all objects 
rm(list = ls()) 

#pfad für unseren projektordner
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

coffeeTable = read.delim("kaffee.asc",sep=" ")

# datentyp korrigieren


#Einzelne Datensätze benennen
row.names(coffeeTable) = paste("T",c(1:130986),"",sep="")

## haushaltsrelevante spalten
data = coffeeTable[,c(3,6,7,8,9,11)]
data[1:10,]

# doppelte datensätze entfernen
data.unique = unique(data)
data.unique[1:10,]

# haushaltsnummer entfernen für clusterbildung, da irrelevant auf bezug der unterschiede der haushalte
data.cluster = data.unique[,c(2:6)]


## clustern
# average linkage

dist.euclid = daisy(data.cluster, metric="euclidean", stand=TRUE)
dendogram = hclust(dist.euclid, method = "average")
plot(dendogram)
#rect.hclust(dendogram, k=1000)

## hinzufügen der clusterzugehörigkeit
cluster.h = cutree(dendogram, k=10) #(maximale kombination der einzelnen merkmale = 1500)
data.cluster.basic = cbind(data.cluster, cluster.h)

## k-means

# standardisieren
data.cluster.stand.alter = (data.cluster$alter - mean(data.cluster$alter))    /sd(data.cluster$alter)
data.cluster.stand.klasse = (data.cluster$klasse - mean(data.cluster$klasse)) /sd(data.cluster$klasse)
data.cluster.stand.einkm = (data.cluster$einkm - mean(data.cluster$einkm))    /sd(data.cluster$einkm)
data.cluster.stand.persn = (data.cluster$persn - mean(data.cluster$persn))    /sd(data.cluster$persn)
data.cluster.stand.bildg = (data.cluster$bildg - mean(data.cluster$bildg))    /sd(data.cluster$bildg)

data.cluster.stand = cbind(data.cluster.stand.alter,data.cluster.stand.bildg,
                           data.cluster.stand.einkm,data.cluster.stand.klasse,data.cluster.stand.persn)

# clustern

clusterzentren = kmeans(data.cluster.stand, centers=10)
clusterzentren

## hinzufügen der clusterzugehörigkeit
data.cluster.kmeans = data.frame(cbind(data.cluster.stand,clusterzentren$cluster))
names(data.cluster.kmeans)[6] = "cluster"


#Datensatz der Haushaltscluster speichern
write.csv(data.cluster.kmeans,"haushalt-kmeans.csv")
write.csv(data.cluster.basic,"haushalt-average-linkage.csv")



