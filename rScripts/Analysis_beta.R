# Load survey and data.table packages
library(survey)
library(data.table)
setwd("C:/Users/guilh/pns2013-lightsmokers")

pnsDT <- readRDS("data/pns.rds")

# Create data.table tabaco without missing data
#tabaco <- pnsDT[ which(pnsDT$P050 != NA)

tabaco <- subset(pnsDT, pnsDT$P050 != " ")

#create variable age
tabaco$idade[18 <= tabaco$C008 & tabaco$C008 < 29]<-0
tabaco$idade[29 <= tabaco$C008 & tabaco$C008 < 59]<-1
tabaco$idade[59 <= tabaco$C008 & tabaco$C008 < 64]<-2
tabaco$idade[64 <= tabaco$C008 & tabaco$C008 < 74]<-3
tabaco$idade[tabaco$C008 >= 74]<-4


#create variable status
tabaco$status[tabaco$P052 == c("3")] <- 0 #nunca fumante  
tabaco$status[tabaco$P05401 == c("2", "3","4")] <- 1 #fumante leve não diario 
tabaco$status[tabaco$P05401 == c("1") & tabaco$P05402 <= 9] <- 2 #fumante leve dia
tabaco$status[tabaco$P05401 == c("1") & tabaco$P05402 > 9] <- 3 #fimante pesado

# Survey format
fumo <- svydesign(
  id = ~1,
  data = tabaco,
  weights = ~V00291
)


# prevalencia de usuario de algum produto de tabaco
prop.table(svytable(formula = ~tabaco$P050, fumo))
prop.table(svytable(formula = ~tabaco$P050+tabaco$status, fumo))



#C006 - Sexo
prop.table(svytable(formula = ~tabaco$C006+tabaco$status+tabaco$C012, fumo))
prop.table(svytable(formula = ~tabaco$C006+tabaco$idade, fumo))
prop.table(svytable(formula = ~tabaco$C006+tabaco$status, fumo))

#VDD004-Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade)
prop.table(svytable(~tabaco$VDD004+tabaco$status, fumo))

