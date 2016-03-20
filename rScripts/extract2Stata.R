# ===============================================
# PNS 2013 - LIGHT ANS HEAVY SMOKERS
# ===============================================
# Aim
#     * This script (1) subsets PNS 2013 data to run multinomial regressions, (2) stores data in stata format.
# Notes 
#     Variables included:
#       Common Variables
#         V00291 - Weight 
#         V0024  - Strata   
#         status - Tobacco status
#         idade2 - Age according to IBGE 2nd classification
#         regiao - Region of Brazil
#           C006 - Sex
#         VDD004 - Education level

#       Model 1
#           N001 - Estado de saúde
#           P032 - Padrão binge de uso de álcool
#           P034 - Atividade física
#           Q002 - Hipertensão 
#           Q030 - Diabetes
#           Q063 - Doenças do coração
#           Q116 - Doenças do pulmão
#           Q120 - Câncer
#           Q092 - Depressão
#           Q132 - Uso de medicamentos para dormir
#           P068 - Fumo passivo 

#        Model 2 - Only smokers
#           P053 - Idade de início
#           P055 - Tempo depois de acordar fuma o primeiro cigarro
#           P060 - Tentativas de parar
#           P061 - Tratamento com profissional de saúde
#           P072 - Advertências

# Open data
pnsDT <- readRDS("data/pns.rds")

# Remove participants who did not filled tobacco survey
tabaco <- subset(pnsDT, pnsDT$P050 != " ")


# Convert variables
## Variable - Status - LIGHT SMOKERS, HEAVY SMOKERS 
tabaco$status[tabaco$P052 == "3"] <- 0  # Never smoker
tabaco$status[tabaco$P05401 == "2" | tabaco$P05401 == "3"| tabaco$P05401 == "4" ]  <- 1 #"Fumante nao diario - cig. ind."
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 <= 10]   <- 2  #"Fumante leve diario - cig. ind."
tabaco$status[tabaco$P05401 == "1" & tabaco$P05402 > 10]    <- 3   # "Fumante pesado - cig. ind."
tabaco$status[tabaco$P050 == 3 & (tabaco$P052 == 1 | tabaco$P052 == 2) ]     <- 4 #"Ex-fumante"
tabaco$status[tabaco$P05401 == 5]                  		 <- 5 #"Nao fumante de cigarro industrializado"

## Variable Age2
# Option to age groups accordoing to recent paper published by IBGE and MS
tabaco$idade2[tabaco$C008>=18 & tabaco$C008 <= 24]<- 0
tabaco$idade2[tabaco$C008>=25 & tabaco$C008 <= 39]<- 1
tabaco$idade2[tabaco$C008>=40 & tabaco$C008 <= 59]<- 2
tabaco$idade2[tabaco$C008 >= 60] <- 3

## Variable region
# Recoding the states into 5 Regions (North, Northest, Central West, Southest and South).
tabaco$regiao[tabaco$V0001 == "11"  | tabaco$V0001 == "12"  | tabaco$V0001 == "13"   | tabaco$V0001 == "14"   | tabaco$V0001 == "15"   | tabaco$V0001 == "16"   | tabaco$V0001 == "17"]<- 0 #norte
tabaco$regiao[tabaco$V0001 == "21"  | tabaco$V0001 == "22"  | tabaco$V0001 == "23"   | tabaco$V0001 == "24"   | tabaco$V0001 == "25" | tabaco$V0001 == "26"  | tabaco$V0001 == "27"  | tabaco$V0001 == "28"  | tabaco$V0001 == "29"  ]<- 1 #nordeste
tabaco$regiao[tabaco$V0001 == "31"  | tabaco$V0001 == "32"  | tabaco$V0001 == "33"   | tabaco$V0001 == "35"]<- 2 #sudeste
tabaco$regiao[tabaco$V0001 == "41"  | tabaco$V0001 == "42"  | tabaco$V0001 == "43"]<- 3 #sul
tabaco$regiao[tabaco$V0001 == "50"  | tabaco$V0001 == "51"  | tabaco$V0001 == "52"   | tabaco$V0001 == "53" ]<- 4 #centro-oeste


# Subset Data
dataModel1 <- tabaco[, c("V00291","V0024","status", "idade2", "regiao","C006","VDD004",
                        "N001", "P032", "P034","Q002","Q030","Q063","Q116","Q120",
                        "Q092","Q132","P068")]

dataModel2 <- tabaco[, c("V00291","V0024","status", "idade2", "regiao","C006","VDD004",
                        "P053", "P055", "P060","P061","P072")]

write.csv(dataModel1, file = "dataModel1.csv")
write.csv(dataModel2, file = "dataModel2.csv")

