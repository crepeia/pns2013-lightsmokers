# =============================
# Read Data from IBGE TXT files
# =============================

# WARNING - Do not run the commands in this section. The data have been already transformed into rds format, which is more compact format.

library(SAScii)
library(data.table)

# Open data. This procedure can take 30 minutes.
pns <- read.SAScii("../Dados/PESPNS2013.txt", "../Dicionаrios e input/input PESPNS2013.sas", buffersize = 50000, beginline = 1)

# Convert data.frame to data.table to improve speed.
pnsDT <- as.data.table(pns) 

# Remove data.frame object and save data.table as rds                         
rm(pns)
saveRDS(pnsDT, "pns.rds")

