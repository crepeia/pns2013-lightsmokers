set.seed(666)


###################
# Libraries
###################

library(caret)
library(rpart)
library(rpart.plot)
library(RWeka)
library(psych)
library(dplyr)
library(neuralnet)
library(ipred)
library(forecast)
library(xts)
library(corrplot)

###################
# Read Data
###################

## Load Train set
train <- read.csv("train.csv", stringsAsFactors = TRUE)
train$Open.Date <- as.POSIXlt("01/01/2015", format="%m/%d/%Y") - as.POSIXlt(train$Open.Date, format="%m/%d/%Y")
train$Open.Date <- as.numeric(train$Open.Date / 1000) #Scale for factors

## Controls
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

## Bagged, random forest, discriminant analysis
ctrlCV <- trainControl(method = "oob",
                       number = 10, repeats = 10)
## Load Test set
test  <- read.csv("test.csv", stringsAsFactors = TRUE)
test$Open.Date <- as.POSIXlt("01/01/2015", format="%m/%d/%Y") - as.POSIXlt(test$Open.Date, format="%m/%d/%Y")
test$Open.Date <- as.numeric(test$Open.Date / 1000) #Scale for factors


###################
# Data Preparation
###################

test <- tabaco[, c("C006", "C009", "P050", "Q092", "Q110", "Q116", "Q120", "Q124" )]
test <- complete.cases(test)

train <- train[, -c(1,3)]
test <- test[, -c(1,3)]

# Creating Dummy Vars
simpleMod <- dummyVars(~ ., data = train)
trainD <- data.frame(predict(simpleMod, train))
simpleMod <- dummyVars(~ ., data = test)
testD <- data.frame(predict(simpleMod, test))


###################
# Data Analysis
###################

# Descriptive analyses. 
# The predictors have different min and max values. As they look different, it requires some normalization.
summary(train[,1:38])

# There is no NA case, so no imputation is required.
lapply(train[,1:38], function(x) table(is.na(x)))

# Inspect whether the sales may be related with time.
train$Open.Date <- as.Date(train$Open.Date, format = "%m/%d/%Y")
timeTrain  <- train[,c("revenue","Open.Date")] 
timeSeries <- xts(timeTrain$revenue, order.by=as.POSIXct(timeTrain$Open.Date))

# Check periodicity
periodicity(timeSeries)
# Check number nmonths(timeSeries)
nmonths(timeSeries)

# Aggregate data into Monthly
test1 <- apply.quarterly(timeSeries, sum)
test1 <- as.data.frame(test1)
timeSeries <- ts(test$V1, start=c(2009,1), end = c(2014,1), frequency = 12)

# Fit data in time series model
fit <- stl(timeSeries, s.window="periodic")

# Plot Data - It seems seasonal.
plot(fit)
monthplot(fit)
seasonplot(timeSeries, col = c("blue","red","green","orange","purple","cyan"))

fitSeries <- fit$time.series
fitSeriesDf  <- data.frame(fitSeries)
fitSeriesDf$date  <- seq(from = as.Date("2009/01/01"), to = as.Date("2014/01/01"), by = "month") 
fitSeriesDf$date <- as.character(fitSeriesDf$date)
fitSeriesDf$date <- substring(fitSeriesDf$date,1,7)

## Combine results from 
train$Open.Date <- as.Date(as.character(train$Open.Date), format = "%m/%d/%Y")
train$Open.Date <- substring(Open.Date,1,7)

train <- merge(train, fitSeriesDf, by.x = "Open.Date", by.y = "date", all.x=TRUE)

# Compare revenue by City.Group
boxplot(train$revenue ~ train$City.Group, horizontal = T)


## Correlations and plots

correlations  <- cor(train[,c(1,4:41)])
corrplot(correlations, order = "hclust")


######################
# REMOVE UNNECESSARY
# PREDICTORS
#####################

## Find highly correlated vars
highCorr <- findCorrelation(correlations, cutoff = .8)
filteredTrain <- train[, -highCorr]
filteredTrain <- filteredTrain[,-2]

###################
# GLM
###################

## 0 MODEL
glmModel_0  <- train(revenue ~ P8 + P20 + P26 + P28, data = train, 
                     method="glm", metric = "RMSE",
                     trControl = ctrl)


## Train Model - glm
glmModel  <- train(revenue ~ . , data = train, 
                   method="glm", metric = "RMSE", 
                   preProcess = "pca",
                   trControl = ctrl)

## Train Model - glm
glmModel_2  <- train(revenue ~ . , data = trainD, 
                     method="glm", metric = "RMSE", 
                     preProcess = c("scale", "center", "pca"),
                     trControl = ctrl)

glmModel        # RESULT - RMSE - 2414545
glmModel_0      #                 2308715
glmModel_2      #                 2359096

###################
# MARS
###################

## Train Model - glm
mars_0  <- train(revenue ~ . , data = train, 
                 method="gcvEarth", metric = "RMSE", 
                 preProcess = c("scale", "center", "pca"),
                 trControl = ctrl)

mars_0         # RESULT - RMSE - 2651139

###################
# CLASSIFICATION 
# REGRESSION TREES
###################

## Classification and Regression Trees
# CART algorithm 
mpartModel  <- train(revenue ~ . , data = train, 
                     method="rpart", metric = "RMSE", 
                     preProcess = c("medianImpute","pca"),
                     trControl = ctrl)
mpartModel # RESULT - RMSE - 2667753.187

###################
# NEURAL NETWORKS
###################

neuralNet  <- train(revenue ~ . , data = train, 
                    method="nnet", metric = "RMSE", 
                    preProcess = c("medianImpute","scale","pca"),
                    trControl = ctrl)
neuralNet # RESULT - RMSE - 5069095.999


###################
# SVM
###################

svmTrain  <- train(revenue ~ . , data = train, 
                   method="svmRadial", metric = "RMSE", 
                   preProcess = c("medianImpute","scale","pca"),
                   trControl = ctrl)

svmTrain # RESULT - RMSE - 2314684.089

###################
# WEKA 5
###################

m5pTrain  <- train(revenue ~ . , data = train, 
                   method="M5", metric = "RMSE", 
                   preProcess = c(,"scale","pca"),
                   trControl = ctrl)

m5pTrain # RESULT - RMSE - 2432554.691

###################
# GBM - BOOST
###################

boostModel  <- train(revenue ~ . , data = train, 
                     method="gbm", metric = "RMSE", 
                     preProcess = c("center","scale","pca"),
                     trControl = ctrl)

boostModel # RESULT - RMSE - 2322672

###################
# BAGGING
###################

bagModel  <- train(revenue ~ . , data = train, 
                   method="treebag", metric = "RMSE", 
                   preProcess = c("center","scale","pca"),
                   trControl = ctrl)

bagModelR_0  <- train(revenue ~ . , data = filteredTrain,
                      method="treebag", metric = "RMSE", 
                      preProcess = c("center","scale","pca"),
                      trControl = ctrl)

bagModelD_0  <- train(revenue ~ . , data = trainD,
                      method="treebag", metric = "RMSE", 
                      preProcess = c("center","scale","pca"),
                      trControl = ctrl)

preProcess(filteredTrain, method = c("center","scale", "pca"),
           pcaComp = 3)

bagModelR_1  <- train(revenue ~ . , data = filteredTrain,
                      method="treebag", metric = "RMSE", 
                      trControl = ctrl)


bagModel         # RESULT - RMSE -  2322039
bagModelR_0      # RESULT - RMSE - 2293945
bagModelR_1                      # 4449082


###################
# RANDOM FOREST
###################

grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
m_rf  <- train(revenue ~ . , data = train, 
               method="rf", metric = "RMSE", 
               preProcess = c("center","scale","pca"),
               trControl = ctrl, tuneGrid = grid_rf)

m_rf_R_1  <- train(revenue ~ . , data = filteredTrain, 
                   method="rf", metric = "RMSE", 
                   trControl = ctrl, tuneGrid = grid_rf)



m_rf # RESULT - RMSE - 2333401
m_rf_R_1

###################
# GENETIC ALGORITHM
###################

ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv",
                       repeats = 5)

rf_ga <- gafs(x = filteredTrain[, 1:11], y = filteredTrain[, 12],
              iters = 200,
              gafsControl = ga_ctrl,
              verbose=TRUE)
rf_ga


###################
# COMPARING RESULTS
###################

results <- resamples(list(ZeroModel = glmModel_0, BAG=bagModel, RF=m_rf, BOOST=boostModel, NN=neuralNet, SVM = svmTrain, LM = glmModel, RegTREES = mpartModel, BAG_Best = bagModelD_0))

summary(results)
bwplot(results)
dotplot(results)

predict.gafs(rf_ga, test)

pred <- predict(bagModelR_1, test)
dfSubmission  <- data.frame(Id = 0:99999, Prediction = exp(pred))
write.csv(dfSubmission, "submission.csv", row.names=FALSE)