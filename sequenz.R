library(psych)
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

# Da die Daten nur innerhalb der Haushalte sequenziell geordnet sind müssen wir den Datensatz erst anpassen

# im Datensatz sind nur 2111 statt 2752 Haushalte???
haushalte = unique(coffeeTable$nummer)

total = c()
treue = c()
for(haushalt in haushalte){
  df = coffeeTable[coffeeTable[,"nummer"] == haushalt,,drop=FALSE]
  total = c(total,nrow(df))
  a = dplyr::count(df,treue)
  rate = 100 / (a[1,2] + a[2,2]) * a[1,2]
  treue = c(treue,rate)
}

## wie treu waren haushalte
hist(treue, breaks=2111/20)


## Häufigkeit der Kaffekaufe pro haushalt
total2haushalt = tibble(c(1:2111),total)
total2haushalt

hist(total2haushalt$total, breaks=2111/20, main="# der Einkäufe pro Haushalt")
#ggplot(total2haushalt,aes(x=c(1:2111),y=total)) + geom_line() + geom_point() + labs(title="Einkauf pro Haushalt", x="haushalt",y="# der einkaufe") + theme_light()







