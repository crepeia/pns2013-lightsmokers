

# Tip - The data is stored as data.table, which is way faster than data.frame. If you do not know how to use data.table, check this out: https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf


# Load survey and data.table packages
library(survey)
library(data.table)

pnsDT <- readRDS("data/pns.rds")
# Survey format
fumo <- svydesign(
  id = ~1,
  data = tabaco,
  weights = ~V00291
)

#criandosubset usuarios de tabago  

fumante_tabaco <- subset(pnsDT, pnsDT$P050 == c("1","2","3"))

# prevalencia de usuario de algum produto de tabaco

fumo <- svydesign(
  id = ~1,
  data = fumante_tabaco,
  weights = ~V00291
  
#encontrando a prevalencia de usuarios de tabaco  
prop.table(svytable(formula = ~fumante_tabaco$P050, fumo))


#criando subset sem missing
tabaco <- subset(pnsDT, pnsDT$P050 != " ")

#create variable status
tabaco$status[tabaco$P052 == c("3")] <- 0 #nunca fumante  
tabaco$status[tabaco$P05401 == c("2", "3")] <- 1 #fumante leve nao diario 
tabaco$status[tabaco$P05401 == c("1") & tabaco$P05402 <= 10] <- 2 #fumante leve dia
tabaco$status[tabaco$P05401 == c("1") & tabaco$P05402 >=11] <- 3 #fimante pesado

#create variable status2
tabaco$status2[tabaco$P052 == c("3")] <- 0 #nunca fumante  
tabaco$status2[tabaco$P05401 == c("2", "3", "4")] <- 1 #fumante leve nao diario 
tabaco$status2[tabaco$P05401 == c("1") & tabaco$P05402 <= 10] <- 2 #fumante leve dia
tabaco$status2[tabaco$P05401 == c("1") & tabaco$P05402 >= 11] <- 3 #fimante pesado


# Survey format
fumo <- svydesign(
  id = ~1,
  data = tabaco,
  weights = ~V00291
)

prop.table(svytable(formula = ~tabaco$status, fumo))
prop.table(svytable(formula = ~tabaco$status2, fumo))
prop.table(svytable(formula = ~tabaco$P050+tabaco$status, fumo))





#create variable age
tabaco$idade[18 <= tabaco$C008 & tabaco$C008 <= 29]<-"18 -29"
tabaco$idade[30 >= tabaco$C008 & tabaco$C008 <= 39]<-"30-39"
tabaco$idade[40 >= tabaco$C008 & tabaco$C008 <= 49]<-"40-49"
tabaco$idade[50 >= tabaco$C008 & tabaco$C008 < 59]<-"50-59"
tabaco$idade[60 >= tabaco$C008 & tabaco$C008 <=64]<"60-64"
tabaco$idade[tabaco$C008 > 64]<-"65 ou mais"



# Survey format
fumo <- svydesign(
  id = ~1,
  data = tabaco,
  weights = ~V00291
)



# prevalencia de usuario de algum produto de tabaco
prop.table(svytable(formula = ~tabaco$status, fumo))
prop.table(svytable(formula = ~tabaco$P050+tabaco$status, fumo))



#C006 - Sexo
prop.table(svytable(formula = ~tabaco$C006+tabaco$status+tabaco$C012, fumo))
prop.table(svytable(formula = ~tabaco$C006+tabaco$idade, fumo))
prop.table(svytable(formula = ~tabaco$C006+tabaco$status, fumo))

#VDD004-N?vel de instru??o mais elevado alcan?ado (pessoas de 5 anos ou mais de idade)
prop.table(svytable(~tabaco$VDD004+tabaco$status, fumo))

