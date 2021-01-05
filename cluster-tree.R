library(rpart)
library(rpart.plot)



###
#  Nicht sicher ob das hier überhaupt sinn macht bei unseren daten..
###

# to remove all objects 
rm(list = ls()) 

#pfad für unseren projektordner
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

coffeeTable = read.delim("kaffee.asc",sep=" ")

#datentyp der einzelnen spalten korrigieren
coffeeTable[[1]] <- as.factor(coffeeTable[[1]])
coffeeTable[[2]] <- as.factor(coffeeTable[[2]])
coffeeTable[[4]] <- as.factor(coffeeTable[[4]])
coffeeTable[[6]] <- as.factor(coffeeTable[[6]])
coffeeTable[[7]] <- as.factor(coffeeTable[[7]])
coffeeTable[[8]] <- as.factor(coffeeTable[[8]])
coffeeTable[[10]] <- as.factor(coffeeTable[[10]])
coffeeTable[[11]] <- as.factor(coffeeTable[[11]])
coffeeTable[[12]] <- as.factor(coffeeTable[[12]])

# numerische Daten standardisieren

dauer.stand = (coffeeTable$dauer - mean(coffeeTable$dauer)) / sd(coffeeTable$dauer)


coffeeTable.stand = data.frame(coffeeTable$treue,coffeeTable$menge,coffeeTable$preis,coffeeTable$nummer,coffeeTable$marke,
                               dauer.stand,coffeeTable$alter,coffeeTable$klasse,coffeeTable$einkm,coffeeTable$persn,coffeeTable$prbew,
                               coffeeTable$bildg)

# entscheidungsbaum (irgendwas stimmt hier gewaltig nicht....)
tree.stand = rpart(coffeeTable.treue ~ .,
             method="class",
             dat=coffeeTable.stand,
             minsplit=2,
             minbucket=1
             
)
rpart.plot(tree.stand, uniform=TRUE, main="Markentreu?", cex=.6)

#vergleich ohne standardisierung
tree = rpart(treue ~ .,
             method="class",
             dat=coffeeTable,
             minsplit=2,
             minbucket=1
)
rpart.plot(tree, uniform=TRUE, main="Markentreu?", cex=.6)
#rpart.plot(tree, type=3, extra=0, branch.lty=3, box.palette="RdYlGn", main="markentreu?", nn=TRUE,cex=.6)
