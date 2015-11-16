# =============================
# ANALYSIS
# =============================
# Some notes - Weight - V00291

# Tip 1 - The data is stored as data.table, which is way faster than data.frame. If you do not know how to use data.table, check this out: https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

# Notas
# 1 - Criar as tabelas deixando a soma entre os sexos em 100% - OK!
# 2 - Recodificar variáveis - OK!
# 3 - Como usar o git apropriadamente - OK!
# 4 - Olhar os gráficos

#setwd("C:/Users/guilh/pns2013-lightsmokers") # This works only on Windows. Please comment this line if you're using a real OS.

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
# TEST 1 - PASSED!!!
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

# Recodificação de todos os participantes selecionados para responder o questionário individual.
# Atenção - Usamos o código comentado abaixo para verificar se todas as categorias atingem 100%. Elas atingem!!

# tabaco$status[tabaco$P052 == "3"]                           <- "Nunca fumante"
# tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- "Fumante leve diario - cig. ind."
# tabaco$status[tabaco$P05401 == "2" | tabaco$P05401 == "3"]  <- "Fumante nao diario - cig. ind." 
# tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 > 10]    <- "Fumante pesado - cig. ind."
# tabaco$status[tabaco$P050 == 3 & 
#                 (tabaco$P052 == 1 | tabaco$P052 == 2) ]     <- "Ex-fumante"
# tabaco$status[tabaco$P05401 == 4]                           <- "Fumante esporadico < 1 vez por mes"
# tabaco$status[tabaco$P05401 == 5]                           <- "Nao fumante de cigarro industrializado"

# VAR status -----
# Recodificação dos participantes das categorias principais do estudo. Light smokers vs everything else.
tabaco$status[tabaco$P052 == "3"]                           <- 0  #"Nunca fumante"
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- 2  #"Fumante leve diario - cig. ind."
tabaco$status[tabaco$P05401 == "2" | tabaco$P05401 == "3"
               | tabaco$P05401 == "4" ]                      <- 1 #"Fumante nao diario - cig. ind."
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 > 10]    <- 3   # "Fumante pesado - cig. ind."
tabaco$status[tabaco$P050 == 3 & (tabaco$P052 == 1 | tabaco$P052 == 2) ]     <- 4 #"Ex-fumante"
tabaco$status[tabaco$P05401 == 5]                           <- 5 #"Nao fumante de cigarro industrializado"

status.names <- c("Nunca Fumante", "Fum. não diário", ...)


## - AGE ----#
tabaco$idade[tabaco$C008>=18 & tabaco$C008 < 29]<- 0
tabaco$idade[tabaco$C008>=29 & tabaco$C008 < 59]<- 1
tabaco$idade[tabaco$C008>=59 & tabaco$C008 < 64]<- 2
tabaco$idade[tabaco$C008>=64 & tabaco$C008 < 74]<- 3
tabaco$idade[tabaco$C008 >= 74]                   <- 4


#Education



######################################################
# The code below does not work out of the box. Needs
# to be validated first.
######################################################

# Status x Sexo
# Tabela Funciona. A soma de cada sexo é 100%
# Aplicar isso em outras variáveis!
round(prop.table(svytable(formula = ~tabaco$status+tabaco$C006,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$C006+tabaco$status,fumo), margin = 2),3)*100

#status x Regi?o do brasil
round(prop.table(svytable(formula = ~tabaco$status+tabaco$V0001,fumo), margin = 2), 3)*100
svyciprop(~tabaco$status, fumo)

#status x faixa etaria 
prop.table(svytable(formula = ~tabaco$status+tabaco$idade,fumo))

#status x escolaridade
prop.table(svytable(formula = ~tabaco$status+tabaco$VDD004,fumo))

#status x hipertens?o
has<-round(prop.table(svytable(formula = ~tabaco$Q002+tabaco$status,fumo), margin=2), 3)*100
has_t <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q002,fumo), margin=2), 3)*100

#status x diabetes 
dm <- round(prop.table(svytable(formula = ~tabaco$Q030+tabaco$status,fumo), margin=2), 3)*100

#status x doen?a ranal cronica 
drc <- round(prop.table(svytable(formula = ~tabaco$Q124+tabaco$status,fumo), margin=2), 3)*100

#status x asma 
asma <- prop.table(svytable(formula = ~tabaco$status2+tabaco$Q074,fumo))

#status x DPOC
dpoc <- prop.table(svytable(formula = ~tabaco$status2+tabaco$Q116,fumo))

#status x c?ncer
cancer <- prop.table(svytable(formula = ~tabaco$status+tabaco$Q120,fumo))

#status x c?ncer de pulm?o
lung <- prop.table(svytable(formula = ~tabaco$status2+tabaco$Q121,fumo))


######################################################
#### GRAPHICS ??
######################################################

fig1 <- rbind(has[2,1:4], dm[2, 1:4], drc[1, 1:4])

