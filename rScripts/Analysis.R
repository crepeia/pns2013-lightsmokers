# =============================
# ANALYSIS
# =============================
# Some notes - Weight - V00291

# Tip 1 - The data is stored as data.table, which is way faster than data.frame. If you do not know how to use data.table, check this out: https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

# Load survey and data.table packages
library(survey)
library(data.table)

# Load packages for graphics
library(reshape2)
library(RColorBrewer)
library(plyr)
library(ggplot2)


# Round output into 3 digits
options(digits=3)

# set R to produce conservative standard errors instead of crashing
options(survey.lonely.psu = "adjust")

# Open data set - BE AWARE - data.table format!!
pnsDT <- readRDS("data/pns.rds")


# Remove participants who did not filled tobacco survey
tabaco <- subset(pnsDT, pnsDT$P050 != " ")




######################################################
# RECODE VARS
######################################################

## - LIGHT SMOKERS ----#

##Recoding all selected participants that answere the individual questionnaire.
tabaco$status[tabaco$P052 == "3"]                       <- 0  #"Nunca fumante"
tabaco$status[tabaco$P05401 == "2" | tabaco$P05401 == "3"| tabaco$P05401 == "4" ]  <- 1 #"Fumante nao diario - cig. ind."
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- 2  #"Fumante leve diario - cig. ind."

tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 > 10]    <- 3   # "Fumante pesado - cig. ind."
tabaco$status[tabaco$P050 == 3 & (tabaco$P052 == 1 | tabaco$P052 == 2) ]     <- 4 #"Ex-fumante"
tabaco$status[tabaco$P05401 == 5]                  		 <- 5 #"Nao fumante de cigarro industrializado"



##FERNANDO - to be done
# status.names <- c("Nunca Fumante", "Fum. não diário", ...)
# Survey format
fumo <- svydesign(
  id = tabaco$UPA,
  strata = tabaco$V0024,
  data = tabaco,
  weights = ~V00291
)
##PREVALENCE OF THE 5 GROUPS
svymean(~tabaco$status, fumo)*100


## - AGE ----#
tabaco$idade[tabaco$C008>=18 & tabaco$C008 < 29]<- 0
tabaco$idade[tabaco$C008>=29 & tabaco$C008 < 59]<- 1
tabaco$idade[tabaco$C008>=59 & tabaco$C008 < 64]<- 2
tabaco$idade[tabaco$C008>=64 & tabaco$C008 < 74]<- 3
tabaco$idade[tabaco$C008 >= 74]        		 <- 4


# Recoding the states into 5 Regions (North, Northest, Central West, Southest and South).
tabaco$regiao[tabaco$V0001 == "11"  | tabaco$V0001 == "12"  | tabaco$V0001 == "13"   | tabaco$V0001 == "14"   | tabaco$V0001 == "15"   | tabaco$V0001 == "16"   | tabaco$V0001 == "17"]<- 0 #norte
tabaco$regiao[tabaco$V0001 == "21"  | tabaco$V0001 == "22"  | tabaco$V0001 == "23"   | tabaco$V0001 == "24"   | tabaco$V0001 == "25" | tabaco$V0001 == "26"  | tabaco$V0001 == "27"  | tabaco$V0001 == "28"  | tabaco$V0001 == "29"  ]<- 1 #nordeste
tabaco$regiao[tabaco$V0001 == "31"  | tabaco$V0001 == "32"  | tabaco$V0001 == "33"   | tabaco$V0001 == "35"]<- 2 #sudeste
tabaco$regiao[tabaco$V0001 == "41"  | tabaco$V0001 == "42"  | tabaco$V0001 == "43"]<- 3 #sul
tabaco$regiao[tabaco$V0001 == "50"  | tabaco$V0001 == "51"  | tabaco$V0001 == "52"   | tabaco$V0001 == "53" ]<- 4
#centro-oeste


# Survey format
fumo <- svydesign(
  id = tabaco$UPA,
  strata = tabaco$V0024,
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
# The code below does not work out of the box. Needs
# to be validated first.
######################################################


####### SOCIODEMOGRAPHIC DATA - tables ##########
# Status x gender
round(prop.table(svytable(formula = ~tabaco$status+tabaco$C006,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$C006+tabaco$status,fumo), margin = 2),3)*100

#test Qui-quadrado
svychisq(formula = ~status+C006,design = fumo,statistic="Chisq")
summary(svytable(formula = ~status+C006,fumo), statistic="Chisq")


#status x brazilian states
round(prop.table(svytable(formula = ~tabaco$status+tabaco$V0001,fumo), margin = 2), 3)*100



#status x Brazilian regions
round(prop.table(svytable(formula = ~tabaco$status+tabaco$regiao,fumo), margin = 2), 3)*100
round(prop.table(svytable(formula = ~tabaco$regiao + tabaco$status,fumo), margin = 2), 3)*100

#test Qui-quadrado
svychisq(formula = ~status+regiao,design=fumo,statistic="Chisq")
summary(svytable(formula = ~status+regiao,fumo), statistic="Chisq")

#status x age
round(prop.table(svytable(formula = ~tabaco$status+tabaco$idade,fumo), margin = 2), 3)*100
round(prop.table(svytable(formula = ~tabaco$idade+tabaco$status, fumo), margin = 2),3)*100

#test Qui-quadrado
svychisq(formula = ~status+idade,design = fumo,statistic="Chisq")
summary(svytable(formula = ~status+idade,fumo), statistic="Chisq")

#status x education level
round(prop.table(svytable(formula = ~tabaco$status+tabaco$VDD004,fumo), margin = 2), 3)*100
round(prop.table(svytable(formula = ~tabaco$VDD004+tabaco$status,fumo), margin = 2),3)*100

#test Qui-quadrado
svychisq(formula = ~status+VDD004, design = fumo,statistic="Chisq")
summary(svytable(formula = ~status+VDD004,fumo), statistic="Chisq")


####### ILLNESS - tables ########

#status x hipertensao

round(prop.table(svytable(formula = ~tabaco$Q002+tabaco$status,fumo), margin=2), 3)*100
has <- round(prop.table(svytable(formula = ~tabaco$Q002+tabaco$status,fumo), margin=2), 3)*100
has_t <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q002,fumo), margin=2), 3)*100


#test Qui-quadrado
svychisq(formula = ~Q002+status,design = fumo,statistic="Chisq")
summary(svytable(formula = ~Q002+status,fumo), statistic="Chisq")

#status x diabetes
round(prop.table(svytable(formula = ~tabaco$Q030+tabaco$status,fumo), margin=2), 3)*100
dm <- round(prop.table(svytable(formula = ~tabaco$Q030+tabaco$status,fumo), margin=2), 3)*100
dm_t <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q030,fumo), margin=2), 3)*100

diabete <- round(prop.table(svytable(formula = ~tabaco$Q030+tabaco$status,fumo), margin=2), 3)*100


#status x doenca renal cronica
round(prop.table(svytable(formula = ~tabaco$Q124+tabaco$status,fumo), margin=2),3)*100
drc <- round(prop.table(svytable(formula = ~tabaco$Q124+tabaco$status,fumo), margin=2),3)*100
drc_t <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q124,fumo), margin=2), 3)*100

#status x asma
round(prop.table(svytable(formula = ~tabaco$Q074+tabaco$status,fumo), margin=2),3)*100
asma <- round(prop.table(svytable(formula = ~tabaco$Q074+tabaco$status,fumo), margin=2),3)*100

#test Qui-quadrado
svychisq(formula = ~Q074+status,design = fumo,statistic="Chisq")
summary(svytable(formula = ~Q074+status,fumo), statistic="Chisq")


summary(fumo, statistic="Chisq")
svychisq(~sch.wide+stype, dclus1, statistic="adjWald")it

#status x DPOC (or emphysema, chronic bronchitis or other)
round(prop.table(svytable(formula = ~tabaco$Q116+tabaco$status,fumo), margin=2),3)*100
dpoc <- round(prop.table(svytable(formula = ~tabaco$Q116+tabaco$status,fumo), margin=2),3)*100


#status x cancer
round(prop.table(svytable(formula = ~tabaco$Q120+tabaco$status,fumo), margin = 2),3)*100
cancer <- round(prop.table(svytable(formula = ~tabaco$Q120+tabaco$status,fumo), margin = 2),3)*100

#test Qui-quadrado
svychisq(formula = ~Q120+status,design = fumo,statistic="Chisq")
summary(svytable(formula = ~Q120+status,fumo), statistic="Chisq")


#test Qui-quadrado
svychisq(formula = ~Q120+status,design = fumo)
summary(svytable(formula = ~Q120+status,fumo), statistic="Chisq")

#status x cancer de pulmao
# OBS: CANCER DE PULMAO - RESPOSTA 1
round(prop.table(svytable(formula = ~tabaco$Q121+tabaco$status,fumo), margin = 2),5)*100
lung <- round(prop.table(svytable(formula = ~tabaco$Q121+tabaco$status,fumo), margin = 2),5)*100


#status x depression
round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q092,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$Q092+tabaco$status,fumo), margin = 2),3)*100

depressão <- round(prop.table(svytable(formula = ~tabaco$Q092+tabaco$status,fumo), margin = 2),3)*100 

#test Qui-quadrado
svychisq(formula = ~status+Q092,design = fumo,statistic="Chisq")
summary(svytable(formula = ~status+Q092fumo), statistic="Chisq")

#status x sleeping pills
round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q132,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$Q132+tabaco$status,fumo), margin = 2),3)*100

medicamento <- round(prop.table(svytable(formula = ~tabaco$Q132+tabaco$status,fumo), margin = 2),3)*100

#status x heart illness
round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q063,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$Q063+tabaco$status,fumo), margin = 2),3)*100

coração <- round(prop.table(svytable(formula = ~tabaco$Q063+tabaco$status,fumo), margin = 2),3)*100
  
#status x AVC
round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q068,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$Q068+tabaco$status,fumo), margin = 2),3)*100

 AVC <- round(prop.table(svytable(formula = ~tabaco$Q068+tabaco$status,fumo), margin = 2),3)*100
###### health status - table #####
#status x heath status
round(prop.table(svytable(formula = ~tabaco$status+tabaco$N001,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$N001+tabaco$status,fumo), margin = 2),3)*100


##### life style - alcohol + physical activity) ########
#status x alcohol use - 1
round(prop.table(svytable(formula = ~tabaco$status+tabaco$P027,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$P027+tabaco$status,fumo), margin = 2),3)*100

#status x alcohol use  - 2  
##recoding variable P028 according to AUDIT

tabaco$diasemana[tabaco$P028==0  | tabaco$P028==1] <- 0   #0 ou 1 vez/semana
tabaco$diasemana[tabaco$P028==2  | tabaco$P028==3] <- 1 #2 ou 3 /semana
tabaco$diasemana[tabaco$P028==4  | tabaco$P028==5 | tabaco$P028==6 | tabaco$P028==7] <- 3 #4 ou mais/semana

round(prop.table(svytable(formula = ~tabaco$diasemana+tabaco$status,fumo), margin = 2),3)*100

round(prop.table(svytable(formula = ~tabaco$status+tabaco$P028,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$P028+tabaco$status,fumo), margin = 2),3)*100

#status x alcohol use - 3 # according to AUDIT

#recoding variable P029 according to AUDIT
tabaco$dose[tabaco$P029==1 | tabaco$P029 ==2]<- 0
tabaco$dose[tabaco$P029==3 | tabaco$P029 ==4]<- 1
tabaco$dose[tabaco$P029==5 | tabaco$P029 ==6]<- 2
tabaco$dose[tabaco$P029==7 | tabaco$P029 ==9]<- 3
tabaco$dose[tabaco$P029>= 10] <-4

round(prop.table(svytable(formula = ~tabaco$dose+tabaco$status,fumo), margin = 2),3)*100

round(prop.table(svytable(formula = ~tabaco$status+tabaco$P029,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$dose+tabaco$status,fumo), margin = 2),3)*100

#status x physical activity
round(prop.table(svytable(formula = ~tabaco$status+tabaco$P034,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$P034+tabaco$status,fumo), margin = 2),3)*100



##### tobacco use - tables ########
#status x tobacco use: history of smoking   #recoding variable P053 according to IBGE graphic

tabaco$idadeinicio[tabaco$P053<=14] <- 0 #menor ou igual a 14
tabaco$idadeinicio[tabaco$P053>=14 & tabaco$P053<=19] <- 1 #15-19 anos
tabaco$idadeinicio[tabaco$P053>=20 & tabaco$P053<=24] <-  2 #20-24 anos
tabaco$idadeinicio[tabaco$P053>=25 & tabaco$P053<=29] <- 3 #25-29 anos
tabaco$idadeinicio[tabaco$P053>=30 & tabaco$P053<=34] <- 4 #30 a 34 anos
tabaco$idadeinicio[tabaco$P053>=35 & tabaco$P053<=39] <- 5 #35 a 39 anos
tabaco$idadeinicio[tabaco$P053>=40] <- 6 #40 ou mais.


round(prop.table(svytable(formula = ~tabaco$idadeinicio+tabaco$status,fumo), margin = 2),3)*100

round(prop.table(svytable(formula = ~tabaco$status+tabaco$P053,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$P053+tabaco$status,fumo), margin = 2),3)*100

#status x tobacco use: how long after wake up, first cigarette
round(prop.table(svytable(formula = ~tabaco$status+tabaco$P055,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$P055+tabaco$status,fumo), margin = 2),3)*100

#status x tobacco use: cessation
round(prop.table(svytable(formula = ~tabaco$status+tabaco$P060,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$P060+tabaco$status,fumo), margin = 2),3)*100

#status x tobacco use: treatment
tableP061 <- subset(tabaco, P061 >=1)
fumoP061 <- svydesign(
  id = tableP061$UPA,
  strata = tableP061$V0024,
  data = tableP061,
  weights = ~V00291
)
round(prop.table(svytable(formula = ~P061+status,fumoP061), margin = 2),3)*100

#status x tobacco use: secondhand smoking
round(prop.table(svytable(formula = ~tabaco$status+tabaco$P068,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$P068+tabaco$status,fumo), margin = 2),3)*100

#status x tobacco use: antitobacco marketing

tableP072 <- subset(tabaco, P072 >=1)
fumoP072 <- svydesign(
  id = tableP072$UPA,
  strata = tableP072$V0024,
  data = tableP072,
  weights = ~V00291
)
round(prop.table(svytable(formula = ~P072+status,fumoP072), margin = 2),3)*100


######################################################
#### GRAPHICS
######################################################

################TESTES - BY TAYNARA##################

### grafico com HIp, diabetes e DRC
fig3 <- rbind(has[2,1:5], dm[2, 1:5], drc[1, 1:5])
fig3 <- data.frame(fig3) # create dataframe
fig3$doencas <- c("Hipertensão", "Diabetes","Doença Renal Crônica") # Add disease name
fig3 <- melt(fig3, id.vars="doencas") # Melt dataFrame to plot on Ggplot, requires reshape2 package
fig3$variable <- revalue(fig3$variable, c("X0"="Nunca Fumante", "X1"="Fumante não diário", "X2"="Fumante Leve", "X3"="Fumante Pesado", "X4"="Ex-fumante")) # Insert names

# Plot graph
ggplot(fig3, aes(x = doencas, y = value, fill=variable)) + # Insert plot basic parameters
  geom_bar(stat="identity", position="dodge") +  # Barplot
  theme_minimal(base_size = 14, base_family = "Arial") + #Font size and Font Family
  xlab("") + ylab("%") + #xlabel and ylabel
  theme(legend.position = "bottom", legend.direction="horizontal",    
        legend.background = element_rect(colour = NA, fill = "white")) + # Postion legend and fill its background with white.
  scale_fill_manual(name="", values = brewer.pal(5, "OrRd")) # Fix legend name and add a better colour pallette



barplot(fig3,beside = TRUE)

#############
# Figure 4 #
#############

# This code creates a dataFrame to plot barcharts
fig4 <- rbind(asma[1, 1:5], dpoc[1, 1:5] ) # Bind data
fig4 <- data.frame(fig4) # create dataframe
fig4$doencas <- c("Asma", "DPOC") # Add disease name
fig4 <- melt(fig4, id.vars="doencas") # Melt dataFrame to plot on Ggplot, requires reshape2 package
fig4$variable <- revalue(fig4$variable, c("X0"="Nunca Fumante", "X1"="Fumante não diário", "X2"="Fumante Leve", "X3"="Fumante Pesado", "X4"="Ex-fumante")) # Insert names

# Plot graph
ggplot(fig4, aes(x = doencas, y = value, fill=variable)) + # Insert plot basic parameters
  geom_bar(stat="identity", position="dodge") +  # Barplot
  theme_minimal(base_size = 14, base_family = "Arial") + #Font size and Font Family
  xlab("") + ylab("%") + #xlabel and ylabel
  theme(legend.position = "bottom", legend.direction="horizontal",
        legend.background = element_rect(colour = NA, fill = "white")) + # Postion legend and fill its background with white.
  scale_fill_manual(name="", values = brewer.pal(5, "OrRd")) # Fix legend name and add a better colour pallette


### figure 5 - AVC and heart illness
# This code creates a dataFrame to plot barcharts
fig5 <- rbind(coração[1, 1:5], AVC[1, 1:5] ) # Bind data
fig5 <- data.frame(fig5) # create dataframe
fig5$doencas <- c("Doença do coração", "AVC") # Add disease name
fig5 <- melt(fig5, id.vars="doencas") # Melt dataFrame to plot on Ggplot, requires reshape2 package
fig5$variable <- revalue(fig5$variable, c("X0"="Nunca Fumante", "X1"="Fumante não diário", "X2"="Fumante Leve", "X3"="Fumante Pesado", "X4"="Ex-fumante")) # Insert names

# Plot graph
ggplot(fig5, aes(x = doencas, y = value, fill=variable)) + # Insert plot basic parameters
  geom_bar(stat="identity", position="dodge") +  # Barplot
  theme_minimal(base_size = 14, base_family = "Arial") + #Font size and Font Family
  xlab("") + ylab("%") + #xlabel and ylabel
  theme(legend.position = "bottom", legend.direction="horizontal",
        legend.background = element_rect(colour = NA, fill = "white")) + # Postion legend and fill its background with white.
  scale_fill_manual(name="", values = brewer.pal(5, "OrRd")) # Fix legend name and add a better colour pallette


### figure 6 - depression and sleeping pills
# This code creates a dataFrame to plot barcharts
fig6 <- rbind(depressão[1, 1:5], medicamento[1, 1:5] ) # Bind data
fig6 <- data.frame(fig6) # create dataframe
fig6$doencas <- c("Depressão", "Uso de medicamentos para dormir") # Add disease name
fig6 <- melt(fig6, id.vars="doencas") # Melt dataFrame to plot on Ggplot, requires reshape2 package
fig6$variable <- revalue(fig6$variable, c("X0"="Nunca Fumante", "X1"="Fumante não diário", "X2"="Fumante Leve", "X3"="Fumante Pesado", "X4"="Ex-fumante")) # Insert names

# Plot graph
ggplot(fig6, aes(x = doencas, y = value, fill=variable)) + # Insert plot basic parameters
  geom_bar(stat="identity", position="dodge") +  # Barplot
  theme_minimal(base_size = 14, base_family = "Arial") + #Font size and Font Family
  xlab("") + ylab("%") + #xlabel and ylabel
  theme(legend.position = "bottom", legend.direction="horizontal",
        legend.background = element_rect(colour = NA, fill = "white")) + # Postion legend and fill its background with white.
  scale_fill_manual(name="", values = brewer.pal(5, "OrRd")) # Fix legend name and add a better colour pallette




### figure 7 - 
# This code creates a dataFrame to plot barcharts
fig7 <- rbind(cancer[1, 1:5], diabete[1, 1:5] ) # Bind data
fig7 <- data.frame(fig7) # create dataframe
fig7$doencas <- c("Câncer", "Diabetes") # Add disease name
fig7 <- melt(fig7, id.vars="doencas") # Melt dataFrame to plot on Ggplot, requires reshape2 package
fig7$variable <- revalue(fig7$variable, c("X0"="Nunca Fumante", "X1"="Fumante não diário", "X2"="Fumante Leve", "X3"="Fumante Pesado", "X4"="Ex-fumante")) # Insert names

# Plot graph
ggplot(fig7, aes(x = doencas, y = value, fill=variable)) + # Insert plot basic parameters
  geom_bar(stat="identity", position="dodge") +  # Barplot
  theme_minimal(base_size = 14, base_family = "Arial") + #Font size and Font Family
  xlab("") + ylab("%") + #xlabel and ylabel
  theme(legend.position = "bottom", legend.direction="horizontal",
        legend.background = element_rect(colour = NA, fill = "white")) + # Postion legend and fill its background with white.
  scale_fill_manual(name="", values = brewer.pal(5, "OrRd")) # Fix legend name and add a better colour pallette



