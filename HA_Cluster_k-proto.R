
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

coffeeTable.stand = coffeeTable
coffeeTable.stand$nummer = (coffeeTable$nummer - mean(coffeeTable$nummer)) / sd(coffeeTable$nummer)
coffeeTable.stand$dauer = (coffeeTable$dauer - mean(coffeeTable$dauer)) / sd(coffeeTable$dauer)
coffeeTable.stand$persn = (coffeeTable$persn - mean(coffeeTable$persn)) / sd(coffeeTable$persn)



library(cluster)
library(FactoMineR)
library(factoextra)
library(clustMixType)
library(nomclust)

### partitionierend ####

#one-hot
oh_data = as.data.frame(model.matrix( ~ . , data = coffeeTable.stand))
oh_data[1] = NULL
colnames(oh_data)[32] = "treue"

# Fehler: kann Vektor der Größe 63.9 GB nicht allozieren
gower.dist = daisy(coffeeTable.stand, metric ="gower")

# Fehler: kann Vektor der Größe 63.9 GB nicht allozieren
gower.dist = daisy(oh_data, metric ="gower")

# Fehler: kann Vektor der Größe 26.8 GB nicht allozieren
gower.dist = daisy(oh_data[1:60000,c(1,2,4,7,8,10,11,12)], metric ="gower")



### anderer anlauf
kpres = kproto(x = coffeeTable.stand, k = 8)
summary(kpres)

clprofiles(kpres, coffeeTable.stand)

Es = numeric(10)
for(i in 1:10){
  kpres = kproto(x = coffeeTable.stand, k = i, nstart = 5)
  Es[i] = kpres$tot.withinss
}

plot(1:10, Es, type = "b", ylab = "Objective Function", xlab = "# Clusters",
     main = "Scree Plot") 


### MCA zur reduzierung auf die wichtigsten features
#Fehler in dimnames(res) <- list(attributes(tab)$row.names, listModa) : 
#  Länge von 'dimnames' [2] ungleich der Arrayausdehnung
mca = MCA(coffeeTable.stand, graph = FALSE)
print(mca)

eigenvalue = get_eigenvalue(mca)
fviz_screenplot(mca, addlabels = TRUE, ylim = c(0,50))



### hierarchisch ####
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

#Fehler: kann Vektor der Größe 127.8 GB nicht allozieren
nomres = nomclust(coffeeTable.stand, prox = TRUE)

#nach 3 std abgebrochen
nomres = nomclust(coffeeTable.stand[1:60000,c(1,2,4,7,8,10,11,12)], prox = TRUE)
plot(nomres$dend)
