library(arules)


# to remove all objects 
rm(list = ls()) 

#pfad für unseren projektordner
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

coffeeTable = read.delim("kaffee.asc",sep=" ")

## datentyp der einzelnen spalten korrigieren
coffeeTable[[1]] <- as.factor(coffeeTable[[1]])
coffeeTable[[2]] <- as.factor(coffeeTable[[2]])
coffeeTable[[4]] <- as.factor(coffeeTable[[4]])
coffeeTable[[6]] <- as.factor(coffeeTable[[6]])
coffeeTable[[7]] <- as.factor(coffeeTable[[7]])
coffeeTable[[8]] <- as.factor(coffeeTable[[8]])
coffeeTable[[10]] <- as.factor(coffeeTable[[10]])
coffeeTable[[11]] <- as.factor(coffeeTable[[11]])
coffeeTable[[12]] <- as.factor(coffeeTable[[12]])

#rows bennenen für übersicht
row.names(coffeeTable) = paste("T",c(1:130986),"",sep="")

coffeeTrans = as(coffeeTable,"transactions")
summary(coffeeTrans)
itemFrequency(coffeeTrans)
itemFrequencyPlot(coffeeTrans)

#### daten auswerten
## verteilung
itemFrequency(coffeeTrans)
itemFrequencyPlot(coffeeTrans)


## Definition:
# The support is the probability that all items in the rule will be
# found together in a checkout basket in relation to the total
# number of transactions.

# The confidence of a rule is the probability of finding socks in a
# checkout basket that contains shoes.


rules = apriori(coffeeTrans, parameter = list(supp=0.15, conf=0.4, target="rules", minlen=2))
inspect(rules)
inspect(sort(rules, by="lift")[1:10])
inspect(sort(rules, by="support")[1:10])
inspect(sort(rules, by="confidence")[1:10])

## top treu
rulesTreu = subset(rules, rhs %in% "treue=1")
inspect(rulesTreu)
inspect(sort(rulesTreu, by="confidence")[1:10])


## Top untreu
rulesUntreu = subset(rules, rhs %in% "treue=2")
inspect(rulesUntreu)
inspect(sort(rulesUntreu, by="confidence")[1:10])
