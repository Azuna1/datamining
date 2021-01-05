library(psych)

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

##
# Verteilung der häufigkeit der kaffe-käufe
hist(coffeeTable$dauer)

# Achtung, dauert ca 20 minuten, ergebnis liegt als .png im ordner bei
#pairs(coffeeTable[c("menge","preis","marke","dauer","klasse","einkm","prbew","bildg","treue")])
#pairs.panels(coffeeTable[c("menge","preis","marke","dauer","klasse","einkm","prbew","bildg","treue")])

model = lm(treue ~ menge + preis + marke + klasse + einkm + prbew + bildg, data = coffeeTable)
model

# Call:
#  lm(formula = treue ~ menge + preis + marke + klasse + einkm + 
#       prbew + bildg, data = coffeeTable)

# Coefficients:
#  (Intercept)       menge2       menge3       preis2       preis3       marke2       marke3       marke4       marke5  
# 1.426573    -0.031655    -0.035908     0.036447    -0.015450     0.136870    -0.100641    -0.022694     0.095066  
# marke6       marke7       marke8       marke9      klasse2      klasse3      klasse4      klasse5       einkm2  
# -0.024482    -0.001081    -0.104034    -0.080752     0.019914     0.031536    -0.013552    -0.016114    -0.008992  
# einkm3       einkm4       prbew2       prbew3       prbew4       bildg2       bildg3  
# -0.015869    -0.030211     0.026460     0.005528     0.029563     0.031281     0.027767  

