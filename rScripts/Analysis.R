# =============================
# ANALYSIS
# =============================
# Some notes - Weight - V00291

# Tip - The data is stored as data.table, which is way faster than data.frame. If you do not know how to use data.table, check this out: https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

# Load survey and data.table packages
library(survey)
library(data.table)

pnsDT <- readRDS("data/pns.rds")

# Create data.table tabaco without missing data
tabaco <- pnsDT[P050 != " "]

# Survey format
sample.pns <- svydesign(
  id = ~1,
  data = tabaco,
  weights = ~V00291
)

# Reproduce the original estimate from IBGE.
# P050 - Atualmente, o(a) Sr(a) fuma algum produto do tabaco?
prop.table(svytable(~P050, sample.pns))

# Recycle Bin
# Example - Recode vars using data.table package
# Assign NA values to cases with value " "
pnsDT[, P050 := ifelse(P050 == " ", NA, P050)]