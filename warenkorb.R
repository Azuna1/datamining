library(arules)
library(arulesViz)
library(tibble)
library(ggplot2)


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
inspect(head(coffeeTrans))

itemFrequency(coffeeTrans)
itemFrequencyPlot(coffeeTrans)



#### daten auswerten

## generelle verteilung 
itemFrequency(coffeeTrans)
itemFrequencyPlot(coffeeTrans)


## Definition:
# The support is the probability that all items in the rule will be
# found together in a checkout basket in relation to the total
# number of transactions.
# -> relative Häufigkeit der Beispiele, in denen die Regel anwendbar ist

# The confidence of a rule is the probability of finding socks in a
# checkout basket that contains shoes.
# ->  relative Häufigkeit der Beispiele, in denen die Regel richtig ist

# Der Lift gibt an, wie hoch der Konfidenzwert für die Regel den Erwartungswert übertrifft,
# er zeigt also die generelle Bedeutung einer Regel

#


rules.2 = apriori(coffeeTrans, parameter = list(supp=0.1, conf=0.1, target="rules", minlen=2))
summary(rules.2)

# THE TOP-N RULES
inspect(head(sort(rules.2, by="lift", decreasing=TRUE),10))

plot(rules.2)

## zum vergleich mit minlen = 3
rules.3 = apriori(coffeeTrans, parameter = list(supp=0.1, conf=0.1, target="rules", minlen=3))
summary(rules.3)

# THE TOP-N RULES
inspect(head(sort(rules.3, by="lift", decreasing=TRUE),10))

plot(rules.3)

## zum vergleich mit minlen = 4
rules.4 = apriori(coffeeTrans, parameter = list(supp=0.1, conf=0.1, target="rules", minlen=4))
summary(rules.4)

# THE TOP-N RULES
inspect(head(sort(rules.4, by="lift", decreasing=TRUE),10))

plot(rules.4)

# minlen = 4 scheint zu komplex, wir nutzen minlen = 3

myVisualSupport = function(df, conf=1, mlen=3){
  minSupport = seq(0.05, 0.9, 0.05)
  total = c()
  for(support in minSupport){
    rules.test = apriori(df, parameter=list(support=support, confidence=conf, minlen=mlen, target="rules"))
    total = c(total,length(rules.test))
  }
  
  rule2support = tibble(minSupport,total)
  rule2support
  
  ggplot(rule2support,aes(x=minSupport,y=total)) + geom_line() + geom_point() + labs(title=sprintf("confidence=%.3f",conf), x="minimum support",y="# of rules") + theme_light()
}

## visualisierung wie sich der support verhält bezogen auf confidence = 0.1
myVisualSupport(coffeeTrans,conf=0.1)

## visualisierung wie sich der support verhält bezogen auf confidence = 0.2
myVisualSupport(coffeeTrans,conf=0.2)

## visualisierung wie sich der support verhält bezogen auf confidence = 0.3
myVisualSupport(coffeeTrans,conf=0.3)

## visualisierung wie sich der support verhält bezogen auf confidence = 0.4
myVisualSupport(coffeeTrans,conf=0.4)

## visualisierung wie sich der support verhält bezogen auf confidence = 0.5
myVisualSupport(coffeeTrans,conf=0.5)

####
# Der support verhält sich jeweils gleich, bis 0.3 confidence werden ca gleich viele regeln gefunden
# über 0.4 reduziert sich die anzahl stark, daher wählen wir für confidence 0.4 und 0.5 im vergleich
# Für support scheint der Bereich zwischen 0.1 und 0.2 interessant, da hier ebenfalls die Anzahl der Regeln stark abnimmt

### support 0.1
rules.3 = apriori(coffeeTrans, parameter = list(supp=0.1, conf=0.4, target="rules", minlen=3))
summary(rules.3)

# THE TOP-N RULES
inspect(head(sort(rules.3, by="lift", decreasing=TRUE),10))

plot(rules.3)

# top treu by lift
rulesTreu = subset(rules.3, rhs %in% "treue=1")
inspect(head(rulesTreu))
inspect(sort(rulesTreu, by="lift")[1:10])


# Top untreu by lift
rulesUntreu = subset(rules.3, rhs %in% "treue=2")
inspect(head(rulesUntreu))
inspect(sort(rulesUntreu, by="lift")[1:10])


### support = 0.15

rules.3 = apriori(coffeeTrans, parameter = list(supp=0.15, conf=0.4, target="rules", minlen=3))
summary(rules.3)

# THE TOP-N RULES
inspect(head(sort(rules.3, by="lift", decreasing=TRUE),10))

plot(rules.3)

# top treu by lift
rulesTreu = subset(rules.3, rhs %in% "treue=1")
inspect(head(rulesTreu))
inspect(sort(rulesTreu, by="lift")[1:10])


# Top untreu by lift
rulesUntreu = subset(rules.3, rhs %in% "treue=2")
inspect(head(rulesUntreu))
inspect(sort(rulesUntreu, by="lift")[1:10])

### support = 0.2

rules.3 = apriori(coffeeTrans, parameter = list(supp=0.2, conf=0.4, target="rules", minlen=3))
summary(rules.3)

# THE TOP-N RULES
inspect(head(sort(rules.3, by="lift", decreasing=TRUE),10))

plot(rules.3)

# top treu by lift
rulesTreu = subset(rules.3, rhs %in% "treue=1")
inspect(head(rulesTreu))
inspect(sort(rulesTreu, by="lift")[1:10])


# Top untreu by lift
rulesUntreu = subset(rules.3, rhs %in% "treue=2")
inspect(head(rulesUntreu))
inspect(sort(rulesUntreu, by="lift")[1:10])

## Marke, Treue
#top treue by lift
test = as(coffeeTable[,c(4,12)],"transactions")
rules.test = apriori(test, parameter = list(supp=0.01, conf=0.1, target="rules", minlen=2))
inspect(head(rules.test))
inspect(head(sort(rules.test, by="lift")))
inspect(head(sort(subset(rules.test, rhs %in% "treue=1"), by="lift")))

#top untreue by lift
inspect(head(sort(subset(rules.test, rhs %in% "treue=2"), by="lift")))

## Menge, Preis, Marke, Preisbewusstsein, Treue
#top treue by lift
test = as(coffeeTable[,c(1,2,4,10,12)],"transactions")
rules.test = apriori(test, parameter = list(supp=0.1, conf=0.1, target="rules", minlen=3))
inspect(head(rules.test))
inspect(head(sort(rules.test, by="lift")))
inspect(head(sort(subset(rules.test, rhs %in% "treue=1"), by="lift")))

#top untreue by lift
inspect(head(sort(subset(rules.test, rhs %in% "treue=2"), by="lift")))

## Alter, Klasse, Einkommen, Treue
#top treue by lift
test = as(coffeeTable[,c(6,7,8,12)],"transactions")
rules.test = apriori(test, parameter = list(supp=0.01, conf=0.1, target="rules", minlen=3))
inspect(head(rules.test))
inspect(head(sort(rules.test, by="lift")))
inspect(head(sort(subset(rules.test, rhs %in% "treue=1"), by="lift")))

#top untreue by lift
inspect(head(sort(subset(rules.test, rhs %in% "treue=2"), by="lift")))

#### falls fehler "Fehler in length(x) : Method length not implemented for class rules"
# in konsole folgende befehle ausführen
# unloadNamespace("arules")
# update.packages("arules")
# library(arules)
