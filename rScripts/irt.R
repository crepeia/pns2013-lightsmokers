# =============================
# ANALYSIS
# =============================

library(data.table)
library(mokken)
library(KernSmoothIRT)

pnsDT <- readRDS("data/pns.rds")


# =============================
# PHQ
# =============================

phq <- pnsDT[, .(N010, N011, N012, N013, N014, N015, N016, N017, N018)]

items <- c("N010", "N011", "N012", "N013", "N014", "N015", "N016", "N017", "N018")

phq[, N010 := ifelse(N010 == " ", NA, N010)]
phq[, N011 := ifelse(N011 == " ", NA, N011)]
phq[, N012 := ifelse(N012 == " ", NA, N012)]
phq[, N013 := ifelse(N013 == " ", NA, N013)]
phq[, N014 := ifelse(N014 == " ", NA, N014)]
phq[, N015 := ifelse(N015 == " ", NA, N015)]
phq[, N016 := ifelse(N016 == " ", NA, N016)]
phq[, N017 := ifelse(N017 == " ", NA, N017)]
phq[, N018 := ifelse(N018 == " ", NA, N018)]

phq9 <- as.data.frame(na.omit(phq))
phq9 <- sapply(phq9, as.numeric)

rm(phq, pnsDT)

# Reliability
check.reliability(phq9)

# Monotonocity
monotonicity.results <- check.monotonicity(phq9)
summary(monotonicity.results)
plot(monotonicity.results)

# IRT
phqIRT <- ksIRT(responses =  phq9, key = 4, format = 2)
phqIRT
plot(phqIRT, plottype = "OCC", items = "all")
plot(phqIRT, plottype = "EIS", items = "all")
plot(phqIRT, plottype = "triangle", items = "all")
plot(phqIRT, plottype = "PCA")
plot(phqIRT, plottype = "RCC", subjects = c(160, 5000, 10000, 15000, 50000))
plot(phqIRT, plottype="expected")
plot(phqIRT, plottype="sd")
plot(phqIRT, plottype="density")

