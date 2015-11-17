# =============================
# ANALYSIS
# =============================
# Some notes - Weight - V00291

# Tip 1 - The data is stored as data.table, which is way faster than data.frame. If you do not know how to use data.table, check this out: https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

# Load survey and data.table packages
library(survey)
library(data.table)

# Round output into 3 digits
options(digits=3)

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
# TEST 1 - 
# Reproduce the original estimates from IBGE.
######################################################

# P050 - Tabaco fumado
prop.table(svytable(formula = ~tabaco$P050, fumo))
# P067 - Outros produtos que não sejam fumados
prop.table(svytable(formula = ~tabaco$P067, fumo))


######################################################
# RECODE VARS
######################################################

## - LIGHT SMOKERS ----#

##Recodificação de todos os participantes selecionados para responder o questionário individual.

tabaco$status[tabaco$P052 == "3"]                           <- 0  #"Nunca fumante"
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- 2  #"Fumante leve diario - cig. ind."
tabaco$status[tabaco$P05401 == "2" | tabaco$P05401 == "3"| tabaco$P05401 == "4" ]                      <- 1 #"Fumante nao diario - cig. ind."
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 > 10]    <- 3   # "Fumante pesado - cig. ind."
tabaco$status[tabaco$P050 == 3 & (tabaco$P052 == 1 | tabaco$P052 == 2) ]     <- 4 #"Ex-fumante"
tabaco$status[tabaco$P05401 == 5]                           <- 5 #"Nao fumante de cigarro industrializado"


##FERNANDO - to be done
# status.names <- c("Nunca Fumante", "Fum. não diário", ...)

##PREVALENCE OF THE 5 GROUPS
svymean(~tabaco$status, fumo)*100 


## - AGE ----#
tabaco$idade[tabaco$C008>=18 & tabaco$C008 < 29]<- 0
tabaco$idade[tabaco$C008>=29 & tabaco$C008 < 59]<- 1
tabaco$idade[tabaco$C008>=59 & tabaco$C008 < 64]<- 2
tabaco$idade[tabaco$C008>=64 & tabaco$C008 < 74]<- 3
tabaco$idade[tabaco$C008 >= 74]                 <- 4


######################################################
# The code below does not work out of the box. Needs
# to be validated first.
######################################################

# Status x Sexo
round(prop.table(svytable(formula = ~tabaco$status+tabaco$C006,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$C006+tabaco$status,fumo), margin = 2),3)*100

#status x Regi?o do brasil
round(prop.table(svytable(formula = ~tabaco$status+tabaco$V0001,fumo), margin = 2), 3)*100

#status x faixa etaria 
round(prop.table(svytable(formula = ~tabaco$status+tabaco$idade,fumo), margin = 2), 3)*100
round(prop.table(svytable(formula = ~tabaco$idade+tabaco$status, fumo), margin = 2),3)*100


#status x escolaridade
round(prop.table(svytable(formula = ~tabaco$status+tabaco$VDD004,fumo), margin = 2), 3)*100
round(prop.table(svytable(formula = ~tabaco$VDD004+tabaco$status,fumo), margin = 2),3)*100


#TABLES - ILLNESS

#status x hipertensao
has <- round(prop.table(svytable(formula = ~tabaco$Q002+tabaco$status,fumo), margin=2), 3)*100
has_t <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q002,fumo), margin=2), 3)*100

#status x diabetes 
dm <- round(prop.table(svytable(formula = ~tabaco$Q030+tabaco$status,fumo), margin=2), 3)*100
dm_t <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q030,fumo), margin=2), 3)*100

#status x doenca ranal cronica 
drc <- round(prop.table(svytable(formula = ~tabaco$Q124+tabaco$status,fumo), margin=2),3)*100
drc_t <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q124,fumo), margin=2), 3)*100

#status x asma 
asma <- round(prop.table(svytable(formula = ~tabaco$Q074+tabaco$status,fumo), margin=2),3)*100

#status x DPOC
dpoc <- round(prop.table(svytable(formula = ~tabaco$Q116+tabaco$status,fumo), margin=2),3)*100

#status x cancer # Do not use for now.
cancer <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q120,fumo), margin = 2),3)*100
                
#status x cancer de pulmao
# OBS: CANCER DE PULMAO - RESPOSTA 1 
lung <- round(prop.table(svytable(formula = ~tabaco$Q121+tabaco$status,fumo), margin = 2),5)*100

######################################################
#### GRAPHICS
######################################################

################TESTES - BY TAYNARA##################

###TESTE - grafico com HIp, diabetes e DRC
fig3 <- rbind(has[2,1:5], dm[2, 1:5], drc[1, 1:5])
barplot(fig3,beside = TRUE)


###TESTE - grafico com ASMA, DpOC E CANCER DE pULMAO 
fig4 <- rbind(asma[1, 1:5], dpoc[1, 1:5] ,lung[2, 1:5])
rownames(fig4) <- c("Asma", "DPOC","Câncer de Pulmão")
colnames(fig4) <- c("Nunca Fumante", "Fumante não diário", "Fumante Leve", "Fumante Pesado", "Ex-fumante")

## GGPLOT2 barChart
ggplot(barreiras, aes(x = reorder(Itens,value), y = value, fill = Tempo)) + geom_bar(stat="identity", position="dodge") + coord_flip() + theme_minimal(base_size = 16, base_family = "Times New Roman") + xlab("") + ylab("")
ggplot(fig4m)

barplot(fig4, beside = TRUE)



