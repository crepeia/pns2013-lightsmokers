# =============================
# ANALYSIS
# =============================
# Some notes - Weight - V00291

# Tip 1 - The data is stored as data.table, which is way faster than data.frame. If you do not know how to use data.table, check this out: https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

setwd("C:/Users/guilh/pns2013-lightsmokers") # This works only on Windows. Please comment this line if you're using a real OS.

# Load survey and data.table packages
library(survey)
library(data.table)

# set R to produce conservative standard errors instead of crashing
options(survey.lonely.psu = "adjust")

# Open data set - BE AWARE - data.table format!!
pnsDT <- readRDS("data/pns.rds")

# Create data.table tabaco without missing data
#tabaco <- pnsDT[ which(pnsDT$P050 != NA)
    
# Remove participants who did not filled tobacco survey
tabaco <- subset(pnsDT, pnsDT$P050 != " ")

# Survey format
fumo <- svydesign(
  id = ~1,
  data = tabaco,
  weights = ~V00291
)


#######################################################
# TEST 1 - PASSED!!!
# Reproduce the original estimates from IBGE.
######################################################

# P050 - Tabaco fumado
prop.table(svytable(formula = ~tabaco$P050, fumo))
# P067 - Outros produtos que nÃ£o sejam fumados
prop.table(svytable(formula = ~tabaco$P067, fumo))


######################################################
# RECODE VARS
######################################################

## - LIGHT SMOKERS ----#

# Please, think of this categories more carefully. We have to be 100% sure.

tabaco$status[tabaco$P052 == "3"]                           <- "Nunca fumante"
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- "Fumante leve diario - cig. ind."
tabaco$status[tabaco$P05401 == "2" | tabaco$P05401 == "3"]  <- "Fumante nao diario - cig. ind." 
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 > 10]    <- "Fumante pesado - cig. ind."
tabaco$status[tabaco$P050 == 3 & 
                (tabaco$P052 == 1 | tabaco$P052 == 2) ]     <- "Ex-fumante"
tabaco$status[tabaco$P05401 == 4]                           <- "Fumante esporadico < 1 vez por mes"
tabaco$status[tabaco$P05401 == 5]                           <- "Nao fumante de cigarro industrializado"

#status2

tabaco$status2[tabaco$P052 == "3"]                           <- "Nunca fumante"
tabaco$status2[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- "Fumante leve diario - cig. ind."
tabaco$status2[tabaco$P05401 == "2" | tabaco$P05401 == "3"]  <- "Fumante nao diario - cig. ind." 
tabaco$status2[tabaco$P05401 == "1" & tabaco$P05402 > 10]    <- "Fumante pesado - cig. ind."


## - AGE ----#
tabaco$idade[18 <= tabaco$C008 & tabaco$C008 < 29]<- 0
tabaco$idade[29 <= tabaco$C008 & tabaco$C008 < 59]<- 1
tabaco$idade[59 <= tabaco$C008 & tabaco$C008 < 64]<- 2
tabaco$idade[64 <= tabaco$C008 & tabaco$C008 < 74]<- 3
tabaco$idade[tabaco$C008 >= 74]                   <- 4


#Education



######################################################
# The code below does not work out of the box. Needs
# to be validated first.
######################################################

#status x Sexo
prop.table(svytable(formula = ~tabaco$status2+tabaco$C006,fumo))

#status x Região do brasil
prop.table(svytable(formula = ~tabaco$status2+tabaco$V0001,fumo))

#status x faixa etaria 
prop.table(svytable(formula = ~tabaco$status2+tabaco$idade,fumo))

#status x escolaridade
prop.table(svytable(formula = ~tabaco$status2+tabaco$VDD004,fumo))

#status x hipertensão
prop.table(svytable(formula = ~tabaco$status2+tabaco$Q002,fumo))


#status x diabetes 
prop.table(svytable(formula = ~tabaco$status2+tabaco$Q030,fumo))

#status x doença ranal cronica 
prop.table(svytable(formula = ~tabaco$status+tabaco$Q124,fumo))

#status x asma 
prop.table(svytable(formula = ~tabaco$status2+tabaco$Q074,fumo))

#status x DPOC
prop.table(svytable(formula = ~tabaco$status2+tabaco$Q116,fumo))

#status x câncer
prop.table(svytable(formula = ~tabaco$status+tabaco$Q120,fumo))

#status x câncer de pulmão
prop.table(svytable(formula = ~tabaco$status2+tabaco$Q121,fumo))


######################################################
#### GRAPHICS ??
######################################################


#hipertensão
barplot(prop.table(svytable(formula = ~tabaco$Q002+tabaco$status2,fumo)),beside = TRUE,main = "Hipertensão")


#diabetes 
barplot(prop.table(svytable(formula = ~tabaco$Q030+tabaco$status2,fumo)),beside = TRUE,main = "Diabetes")

#doença ranal cronica 
barplot(prop.table(svytable(formula = ~tabaco$Q124+tabaco$status2,fumo)),beside = TRUE,main = "Doença renal Crônica")

#Asma 
barplot(prop.table(svytable(formula = ~tabaco$Q074+tabaco$status2,fumo)),beside = TRUE,main = "Asma")

#status x DPOC 
barplot(prop.table(svytable(formula = ~tabaco$Q116+tabaco$status2,fumo)),beside = TRUE,main = "DPOC")

#status x câncer
barplot(prop.table(svytable(formula = ~tabaco$Q120+tabaco$status2,fumo)),beside = TRUE,main = "Câncer")

#status x tipo de câncer
barplot(prop.table(svytable(formula = ~tabaco$Q121+tabaco$status2,fumo)),beside = TRUE,main = "tipo de Câncer ")

