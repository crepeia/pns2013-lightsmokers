# ===============================================
# PNS 2013 - LIGHT ANS HEAVY SMOKERS
# ===============================================
# Aim
#     * Let's use polynomial regression on R
#     * This script is only sandbox. Do no take it seriously.
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

set.seed(666)

library(survey)
library(mlogit)

###################
# Read Data
###################

## Load data
dataModel1 <- read.csv("dataModel1.csv", stringsAsFactors = TRUE, colClasses = "factor", na.strings = " ")
dataModel1 <- dataModel1[,-c(1,2,3)]
levels(dataModel1$status) <-c("Never Smoker","Not daily smoker", "Light daily smoker", "Heavy smoker", "Former smoker", "Does not use regular cigarettes")
levels(dataModel1$regiao) <-c("North","Northeast", "Southeast", "South", "Midwest")

# Survey design
svyDesign <- svydesign(
  id = dataModel1$UPA,
  strata = dataModel1$V0024,
  data = dataModel1,
  weights = ~V00291
)

# Regression Model
m0 <- mlogit(status ~ idade2 + regiao, data = dataModel1, weights = V00291, shape = "wide", choice = "mode")
