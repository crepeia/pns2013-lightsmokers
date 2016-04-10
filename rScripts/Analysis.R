# ===============================================
# ANALYSIS PNS 2013 - LIGHT ANS HEAVY SMOKERS
# ===============================================
# Notes 
#      1 - ID - UPA; Weight - $V00291; Strata - $V0024
#      2 - The data is stored as data.table to do things faster.

######################################################
# LOAD PACKAGES
######################################################

# Load survey and data.table packages
library(survey)

# Load packages for graphics
library(reshape2)
library(RColorBrewer)
library(plyr)
library(ggplot2)

######################################################
# SET GENERAL OPTIONS
######################################################
# Round output into 3 digits
options(digits=3)
# set R to produce conservative standard errors instead of crashing
options(survey.lonely.psu = "adjust")

######################################################
# LOAD DATA
######################################################
# Open data set - NOTE - data is stored as data.table!
pnsDT <- readRDS("data/pns.rds")

# Remove participants who did not filled tobacco survey
tabaco <- subset(pnsDT, pnsDT$P050 != " ")
rm(pnsDT)

######################################################
# RECODE VARS
######################################################

## - LIGHT SMOKERS, HEAVY SMOKERS ----#
# Recode participants who answered the individual questionnaire.
tabaco$status[tabaco$P052 == "3"] <- 0  # Never smoker
tabaco$status[tabaco$P05401 == "2" | tabaco$P05401 == "3"| tabaco$P05401 == "4" ]  <- 1 #"Fumante nao diario - cig. ind."
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- 2  #"Fumante leve diario - cig. ind."

tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 > 10]    <- 3   # "Fumante pesado - cig. ind."
tabaco$status[tabaco$P050 == 3 & (tabaco$P052 == 1 | tabaco$P052 == 2) ]     <- 4 #"Ex-fumante"
tabaco$status[tabaco$P05401 == 5]                  		 <- 5 #"Nao fumante de cigarro industrializado"


# Recode age into grupos according to IBGE publication
tabaco$idade2[tabaco$C008>=18 & tabaco$C008 <= 24]<- 0
tabaco$idade2[tabaco$C008>=25 & tabaco$C008 <= 39]<- 1
tabaco$idade2[tabaco$C008>=40 & tabaco$C008 <= 59]<- 2
tabaco$idade2[tabaco$C008 >= 60] <- 3


# Recode 27 states into 5 regions (North, Northest, Central West, Southest and South).
tabaco$regiao[tabaco$V0001 == "11"  | tabaco$V0001 == "12"  | tabaco$V0001 == "13"   | tabaco$V0001 == "14"   | tabaco$V0001 == "15"   | tabaco$V0001 == "16"   | tabaco$V0001 == "17"]<- 0 #norte
tabaco$regiao[tabaco$V0001 == "21"  | tabaco$V0001 == "22"  | tabaco$V0001 == "23"   | tabaco$V0001 == "24"   | tabaco$V0001 == "25" | tabaco$V0001 == "26"  | tabaco$V0001 == "27"  | tabaco$V0001 == "28"  | tabaco$V0001 == "29"  ]<- 1 #nordeste
tabaco$regiao[tabaco$V0001 == "31"  | tabaco$V0001 == "32"  | tabaco$V0001 == "33"   | tabaco$V0001 == "35"]<- 2 #sudeste
tabaco$regiao[tabaco$V0001 == "41"  | tabaco$V0001 == "42"  | tabaco$V0001 == "43"]<- 3 #sul
tabaco$regiao[tabaco$V0001 == "50"  | tabaco$V0001 == "51"  | tabaco$V0001 == "52"   | tabaco$V0001 == "53" ]<- 4#centro-oeste


# Recode educational levels into "less than 8 years" and "8 years or more"
tabaco$educa[tabaco$VDD004 == "1" | tabaco$VDD004 =="2"  | tabaco$VDD004 == "3" | tabaco$VDD004 == "4"]<-0 # Less than 8 years
tabaco$educa[tabaco$VDD004 == "5"  | tabaco$VDD004 == "6" | tabaco$VDD004 =="7"]<-1 # 8 or more years


# Recode health status into "good and very goor" or "regular, bad, really bad".
tabaco$saude[tabaco$N001 == "1"  | tabaco$N001 == "2"]<-0                       # Good or very good health
tabaco$saude[tabaco$N001 == "3"  | tabaco$N001 == "4"| tabaco$N001 == "5"]<-1   # Regular or poor health

# Alcohol use  - days per week - Recode variable P028 according to AUDIT
tabaco$alcohol[tabaco$P028== 0  | tabaco$P028==1] <- 0                                     # None or once a week
tabaco$alcohol[tabaco$P028==2  | tabaco$P028==3] <- 1                                      # 2 or 3 times a week
tabaco$alcohol[tabaco$P028==4  | tabaco$P028==5 | tabaco$P028==6 | tabaco$P028==7] <- 2    # 4 or more times a week

#---------------------------
# Create proper levels
#---------------------------

## status
tabaco$status <- as.factor(tabaco$status)
levels(tabaco$status) <-c("never.smoker","not.daily.smoker", "light.smoker", "heavy.smoker", "former.smoker", "not.regular.cigarettes")

## regiao
tabaco$regiao <- as.factor(tabaco$regiao)
levels(tabaco$regiao) <-c("North","Northeast", "Southeast", "South", "Midwest")

# Sex
tabaco$C006 <- as.factor(tabaco$C006)
levels(tabaco$C006) <-c("Male","Female")

# Educational level
tabaco$educa <- as.factor(tabaco$educa)
levels(tabaco$educa) <-c("less than 8","8 or more")

# Age according to IBGE
tabaco$idade2 <- as.factor(tabaco$idade2)

# Educational level
tabaco$VDD004 <- as.factor(tabaco$VDD004)
levels(tabaco$VDD004) <-c("sem.instrucao","fundamental.incompleto","fundamental.completo", "medio.incompleto", "medio.completo","superior.incompleto","superior.completo")

# Hypertension
tabaco$Q002 <- as.factor(tabaco$Q002)
levels(tabaco$Q002) <-c(NA, "yes","only.during.pregnancy","no")

# Diabetes
tabaco$Q030 <- as.factor(tabaco$Q030)
levels(tabaco$Q030) <-c(NA, "yes","only.during.pregnancy","no")

# Chronic kidney disease
tabaco$Q124 <- as.factor(tabaco$Q124)
levels(tabaco$Q124) <-c("yes","no")

# Asthma
tabaco$Q074 <- as.factor(tabaco$Q074)
levels(tabaco$Q074) <-c("yes","no")

# Lung diseases
tabaco$Q116 <- as.factor(tabaco$Q116)
levels(tabaco$Q116) <-c("yes","no")

# Cancer
tabaco$Q120 <- as.factor(tabaco$Q120)
levels(tabaco$Q120) <-c("yes","no")

# Types of cancer
tabaco$Q121 <- as.factor(tabaco$Q121)
levels(tabaco$Q121) <-c(NA, "Lung","Bowel","Stomach","Breast","Cervical","Prostate","Skin","Other")

# Depression
tabaco$Q092 <- as.factor(tabaco$Q092)
levels(tabaco$Q092) <-c("yes","no")

# Sleeping pills
tabaco$Q132 <- as.factor(tabaco$Q132)
levels(tabaco$Q132) <-c("yes","no")

# Heart disease
tabaco$Q063 <- as.factor(tabaco$Q063)
levels(tabaco$Q063) <-c("yes","no")

# Stroke
tabaco$Q068 <- as.factor(tabaco$Q068)
levels(tabaco$Q068) <-c("yes","no")

# Alcohol use - days per week
tabaco$diasemana <- as.factor(tabaco$diasemana)
levels(tabaco$diasemana) <-c("0 or 1","2 or three", "4 or more")

# Recode health status into "good and very goor" or "regular, bad, really bad".
tabaco$saude <- as.factor(tabaco$saude)
levels(tabaco$saude) <-c("Good or very good", "Regular, poor or really poor health")

# Alcohol use  - days per week - Recode variable P028 according to AUDIT
tabaco$alcohol <- as.factor(tabaco$alcohol)
levels(tabaco$alcohol) <-c("None or once a week", "2 or 3 times a week", "4 or more times a week")

# Binge drinking
tabaco$P032 <- as.factor(tabaco$P032)
levels(tabaco$P032) <-c(NA,"Yes", "No")

# Physical activity
tabaco$P034 <- as.factor(tabaco$P034)
levels(tabaco$P032) <-c("Yes", "No")


######################################################
# SURVEY DESIGN
######################################################

# Survey format
fumo <- svydesign(
  id = tabaco$UPA,
  strata = tabaco$V0024,
  data = tabaco,
  weights = ~V00291
)

#######################################################
# Reproduce the original estimates from IBGE.
######################################################

tableCI <- function(x,y){
  round(cbind("Percentage" = svymean(~x, fumo), confint(svymean(~x, y), df=degf(y))),4)*100
}

# P050 - Tabaco fumado
tableCI(tabaco$P050, fumo)

# P067 - Outros produtos que não sejam fumados
tableCI(tabaco$P067, fumo)


#Prevalence of each group
prop.table(svytable(formula = ~tabaco$status, fumo))*100


##Prevalence of each tobacco group
#recoding variables only for smokers
tabaco$status2[tabaco$P05401 == "2" | tabaco$P05401 == "3"| tabaco$P05401 == "4" ]  <- 1 #"Fumante nao diario - cig. ind."
tabaco$status2[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- 2  #"Fumante leve diario - cig. ind."

tabaco$status2[tabaco$P05401 == "1" & tabaco$P05402 > 10]    <- 3   # "Fumante pesado - cig. ind."

prop.table(svytable(formula = ~tabaco$status2, fumo))*100

#Status x gender
## New version with Standard errors
round(ftable(svyby(~C006, ~status ,  design =fumo, FUN = svymean, keep.var = TRUE))*100,1)
## New version with CI's. I don't think it's a good idea to use CI's a good idea because we will have to much info to display on tables.

#######################################################
# TABLES - SOCIO DEMOGRAPHIC
######################################################

# STATUS VS. SEX
# Table with SE
round(ftable(svyby(~C006, ~status,  design =fumo, FUN = svymean, keep.var = TRUE))*100,1)
#Chi-square test
svychisq(formula = ~status+C006,design = fumo,statistic="Chisq")


# STATUS VS. REGIONS
# Table with SE
round(ftable(svyby(~regiao, ~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)
#Chi-square test
svychisq(formula = ~status+regiao,design=fumo,statistic="Chisq")


#STATUS VS. AGE
round(ftable(svyby(~idade2, ~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)
#Chi-square test
svychisq(formula = ~status+idade2,design=fumo,statistic="Chisq")


#STATUS VS. EDUCATIONAL LEVEL
# Var1
round(ftable(svyby(~status, ~VDD004 ,  design =fumo, FUN = svymean, keep.var = TRUE))*100,1)
#Chi-square test
svychisq(formula = ~status+ VDD004,design=fumo,statistic="Chisq")
# Var2 
round(ftable(svyby(~educa, ~status,   design =fumo, FUN = svymean, keep.var = TRUE))*100,1)

######################################################
# ILLNESS - TABLES
######################################################

# STATUS x HYPERTENSION
round(ftable(svyby(~status, ~Q002 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q002+status, design = fumo, statistic="Chisq")


# STATUS x DIABETES
round(ftable(svyby(~status, ~Q030 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q030+status, design = fumo, statistic="Chisq")


# STATUS x CHRONIC KIDNEY DISEASE
round(ftable(svyby(~status, ~Q124 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q124+status, design = fumo, statistic="Chisq")


# STATUS X ASTHMA
round(ftable(svyby(~status, ~Q074 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q074+status, design = fumo, statistic="Chisq")


# STATUS X LUNG DISEASES 
round(ftable(svyby(~status, ~Q116 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q116+status, design = fumo, statistic="Chisq")


# STATUS CANCER
round(ftable(svyby(~status, ~Q120 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q120+status, design = fumo, statistic="Chisq")


# STATUS X FIRST DIAGNOSED CANCER
round(ftable(svyby(~status, ~Q121 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q121+status, design = fumo, statistic="Chisq")


# STATUS X DEPRESSION
round(ftable(svyby(~status, ~Q092 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q092+status, design = fumo, statistic="Chisq")


# STATUS X SLEEPING PILLS
round(ftable(svyby(~status, ~Q132 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q132+status, design = fumo, statistic="Chisq")


# STATUS X HEART DISEASES
round(ftable(svyby(~status, ~Q063 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q063+status, design = fumo, statistic="Chisq")


# STATUS X STROKE
round(ftable(svyby(~status, ~Q068 ,  design = fumo, FUN = svymean, keep.var = TRUE))*100,1)
# Chi-square test
svychisq(formula = ~Q068+status, design = fumo, statistic="Chisq")

######################################################
# HEALTH STATUS - TABLES
######################################################

# STATUS x HEALTH STATUS
round(ftable(svyby( ~tabaco$saude, ~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)


# ALCOHOL USE X STATUS
round(ftable(svyby( ~alcohol, ~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)


# BINGE X STATUS
round(ftable(svyby( ~P032, ~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)


# PHYSICAL ACTIVITY X STATUS
round(ftable(svyby( ~P034, ~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)

######################################################
# TOBACCO USE - TABLES
######################################################

#status x tobacco use: history of smoking  
#age groups according to IBGE  recent paper
tabaco$idadeinicio2[tabaco$P053<18] <- 0 #menor ou igual a 18
tabaco$idadeinicio2[tabaco$P053>=18 & tabaco$P053<=24] <- 1 #18-24 anos
tabaco$idadeinicio2[tabaco$P053>=25] <-  2 #25 ou mais

round(prop.table(svytable(formula = ~tabaco$idadeinicio2+tabaco$status,fumo), margin = 2),3)*100
#WITH SE
round(ftable(svyby(~tabaco$idadeinicio2, ~status,  design =fumo, FUN = svymean, keep.var = TRUE))*100,1)


#status x tobacco use: how long after wake up, first cigarette
round(prop.table(svytable(formula = ~tabaco$P055+tabaco$status,fumo), margin = 2),3)*100
#with SE
round(ftable(svyby(~P055,~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)


#status x tobacco use: cessation
round(prop.table(svytable(formula = ~tabaco$P060+tabaco$status,fumo), margin = 2),3)*100

#WITH se
round(ftable(svyby(~P060,~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)


#status x tobacco use: treatment
#using this code to eliminate non-applicable results
tableP061 <- subset(tabaco, P061 >=1)
fumoP061 <- svydesign(
  id = tableP061$UPA,
  strata = tableP061$V0024,
  data = tableP061,
  weights = ~V00291
)
round(prop.table(svytable(formula = ~P061+status,fumoP061), margin = 2),3)*100

#with SE
round(ftable(svyby(~P061, ~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)

#status x tobacco use: secondhand smoking
round(prop.table(svytable(formula = ~tabaco$P068+tabaco$status,fumo), margin = 2),3)*100

#recoding secondhand into "daily" x "nondaily"
tabaco$passivo[tabaco$P068 == "1"]<-0 #daily
tabaco$passivo[tabaco$P068 == "2" |tabaco$P068 == "3" |tabaco$P068 == "4"]<-1 #less than daily
tabaco$passivo[tabaco$P068 == "5"]<-2 #never

round(prop.table(svytable(formula = ~tabaco$passivo+tabaco$status,fumo), margin = 2),3)*100

#with SE
round(ftable(svyby( ~tabaco$passivo, ~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)


#status x tobacco use: antitobacco marketing
#using this code to eliminate non-applicable results
tableP072 <- subset(tabaco, P072 >=1)
fumoP072 <- svydesign(
  id = tableP072$UPA,
  strata = tableP072$V0024,
  data = tableP072,
  weights = ~V00291
)
round(prop.table(svytable(formula = ~P072+status,fumoP072), margin = 2),3)*100

#with SE
round(ftable(svyby(~P072,~status, design =fumo, FUN = svymean, keep.var = TRUE))*100,1)


#P051- NON-DAILY SMOKERS THAT were daily smokers in the past
#using this code to eliminate non-applicable results
tableP051 <- subset(tabaco, P051 >=1)
fumoP051 <- svydesign(
  id = tableP051$UPA,
  strata = tableP051$V0024,
  data = tableP051,
  weights = ~V00291
)
round(prop.table(svytable(formula = ~P051+status,fumoP051), margin = 2),3)*100


######################################################
#### GRAPHICS
######################################################

###GRAPHIC 1: HYPERTENSION, DIABETES, RENAL DISEASE
fig3 <- rbind(has[2,1:5], dm[2, 1:5], drc[1, 1:5])
fig3 <- data.frame(fig3) # create dataframe
fig3$doencas <- c("Hipertensão", "Diabetes","Insuficiência Renal Crônica") # Add disease name
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


##graphic 2 : ASTHMA AND COPD

# This code creates a dataFrame to plot barcharts
fig4 <- rbind(asma[1, 1:5], dpoc[1, 1:5] ) # Bind data
fig4 <- data.frame(fig4) # create dataframe
fig4$doencas <- c("Asma", "Doenças pulmonares") # Add disease name
fig4 <- melt(fig4, id.vars="doencas") # Melt dataFrame to plot on Ggplot, requires reshape2 package
fig4$variable <- revalue(fig4$variable, c("X0"="Nunca Fumante", "X1"="Fumante não diário", "X2"="Fumante Leve", "X3"="Fumante Pesado", "X4"="Ex-fumante")) # Insert names

# Plot graph
ggplot(fig4, aes(x = doencas, y = value, fill=variable)) + # Insert plot basic parameters
  geom_bar(stat="identity", position="dodge") +  # Barplot
  theme_minimal(base_size = 14, base_family = "Arial") + #Font size and Font Family
  xlab("") + ylab("%") + #xlabel and ylabel
  theme(legend.position = "bottom", legend.direction="horizontal", depres,        legend.background = element_rect(colour = NA, fill = "white")) + # Postion legend and fill its background with white.
  scale_fill_manual(name="", values = brewer.pal(5, "OrRd")) # Fix legend name and add a better colour pallette


### GRAPHIC 3 : VCA AND HEART ILLNESS
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


### GRAPHIC 3: VDEPRESSION, SLEEPING PILLS AND CANCER
# This code creates a dataFrame to plot barcharts
fig6 <- rbind(depressão[1, 1:5], medicamento[1, 1:5], cancer[1, 1:5] ) # Bind data
fig6 <- data.frame(fig6) # create dataframe
fig6$doencas <- c("Depressão", "Uso de medicamentos para dormir", "Câncer") # Add disease name
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


#### HENRIQUE's EXPERIMENTAL CODE #####
df  <- data.frame(round(ftable(svyby(~status, ~C006 ,  design =fumo, FUN = svymean, keep.var = TRUE))*100,1))
dfCast <- dcast(df, Var3 + C006 ~ Var2)
table1 <- ftable(svyby(~status, ~C006 ,  design =fumo, FUN = svymean, keep.var = TRUE))

## Old version without Standard Errors
round(prop.table(svytable(formula = ~tabaco$status+tabaco$C006,fumo), margin = 2),3)*100
