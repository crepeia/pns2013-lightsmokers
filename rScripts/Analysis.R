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

##Recoding all selected participants that answere the individual questionnaire.
tabaco$status[tabaco$P052 == "3"]                           <- 0  #"Nunca fumante"
tabaco$status[tabaco$P05401 == "2" | tabaco$P05401 == "3"| tabaco$P05401 == "4" ]  <- 1 #"Fumante nao diario - cig. ind."
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- 2  #"Fumante leve diario - cig. ind."

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

# Status x gender
round(prop.table(svytable(formula = ~tabaco$status+tabaco$C006,fumo), margin = 2),3)*100
round(prop.table(svytable(formula = ~tabaco$C006+tabaco$status,fumo), margin = 2),3)*100

#status x brazilian states
round(prop.table(svytable(formula = ~tabaco$status+tabaco$V0001,fumo), margin = 2), 3)*100

#status x age 
round(prop.table(svytable(formula = ~tabaco$status+tabaco$idade,fumo), margin = 2), 3)*100
round(prop.table(svytable(formula = ~tabaco$idade+tabaco$status, fumo), margin = 2),3)*100


#status x education level
round(prop.table(svytable(formula = ~tabaco$status+tabaco$VDD004,fumo), margin = 2), 3)*100
round(prop.table(svytable(formula = ~tabaco$VDD004+tabaco$status,fumo), margin = 2),3)*100


#TABLES - ILLNESS

#status x hipertensao
has <- round(prop.table(svytable(formula = ~tabaco$Q002+tabaco$status,fumo), margin=2), 3)*100
has_t <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q002,fumo), margin=2), 3)*100

#status x diabetes 
dm <- round(prop.table(svytable(formula = ~tabaco$Q030+tabaco$status,fumo), margin=2), 3)*100
dm_t <- round(prop.table(svytable(formula = ~tabaco$status+tabaco$Q030,fumo), margin=2), 3)*100

#status x doenca renal cronica 
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
fig3 <- data.frame(fig3) # create dataframe
fig3$doencas <- c("Hiertensão", "Diabetes","Doença Renal Crônica") # Add disease name
fig3 <- melt(fig3, id.vars="doencas") # Melt dataFrame to plot on Ggplot, requires reshape2 package
fig3$variable <- revalue(fig3$variable, c("X0"="Nunca Fumante", "X1"="Fumante não diário", "X2"="Fumante Leve", "X3"="Fumante Pesado", "X4"="Ex-fumante")) # Insert names

# Plot graph
ggplot(fig3, aes(x = doencas, y = value, fill=variable)) + # Insert plot basic parameters 
  geom_bar(stat="identity", position="dodge") +  # Barplot
  theme_minimal(base_size = 14, base_family = "Arial") + #Font size and Font Family
  xlab("") + ylab("%") + #xlabel and ylabel
  theme(legend.position = c(.8,.8), legend.background = element_rect(colour = NA, fill = "white")) + # Postion legend and fill its background with white.
  scale_fill_manual(name="Legenda", values = brewer.pal(5, "OrRd")) # Fix legend name and add a better colour pallette



barplot(fig3,beside = TRUE)

#############
# Figure 4 #
#############

# This code creates a dataFrame to plot barcharts
fig4 <- rbind(asma[1, 1:5], dpoc[1, 1:5] ,lung[2, 1:5]) # Bind data
fig4 <- data.frame(fig4) # create dataframe
fig4$doencas <- c("Asma", "DPOC","Câncer de Pulmão") # Add disease name
fig4 <- melt(fig4, id.vars="doencas") # Melt dataFrame to plot on Ggplot, requires reshape2 package
fig4$variable <- revalue(fig4$variable, c("X0"="Nunca Fumante", "X1"="Fumante não diário", "X2"="Fumante Leve", "X3"="Fumante Pesado", "X4"="Ex-fumante")) # Insert names

# Plot graph
ggplot(fig4, aes(x = doencas, y = value, fill=variable)) + # Insert plot basic parameters 
     geom_bar(stat="identity", position="dodge") +  # Barplot
     theme_minimal(base_size = 14, base_family = "Arial") + #Font size and Font Family
     xlab("") + ylab("%") + #xlabel and ylabel
     theme(legend.position = c(.8,.8), legend.background = element_rect(colour = NA, fill = "white")) + # Postion legend and fill its background with white.
     scale_fill_manual(name="Legenda", values = brewer.pal(5, "OrRd")) # Fix legend name and add a better colour pallette

