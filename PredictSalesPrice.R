
###
# Set working directory
###
setwd("D:/ujjwal/Tutorial/DataScience/Imarticus/Group Projects/R-ImarticusProject")

# install.packages("dplyr")
# install.packages('caret')
# install.packages('doSNOW')
# install.packages('snow')
library(caret)
library(dplyr)
library(mice)
library(snow)
library(doSNOW)

source("cleanData.R")

# Assign the data file name here.

DataFileName <- 'data\\Group Project on R-Data Set-4.csv'
# DataFileName <- 'data\\Gropup Project for R-Data Set.csv'

raw_data <- read.csv(DataFileName, stringsAsFactors = TRUE)

# Make a copy of raw data 
dataset <- raw_data

# Let's see the data, how it looks
head(dataset)

# Summary of dataset will give us the picture how data is distributed.
summary(dataset)

# List down all factors in the dataset
# names(Filter(is.numeric, dataset))

###
# Step - Data cleaning
# Handle missing values
###
dataset <- processNA(dataset, 0.4)

# Confirm wheather all missing values are removed
md.pattern(dataset)

# Let's list down all factors in the dataset
# names(Filter(is.factor, dataset))

split<-createDataPartition(y = dataset$SalePrice, 
                           # times = 1, 
                           p = 0.85,
                           list = FALSE)
dataset.train <- dataset[ split, ]
dataset.validate <- dataset[ -split, ]

# Examin the proportions of the split on SalePrice was done in similar fashion
# prop.table(table(dataset$SalePrice))
# prop.table(table(dataset.train$SalePrice))
# prop.table(table(dataset.validate$SalePrice))

###
# Create linear model
###
lmFit <- train( SalePrice ~ ., 
                data = dataset.train, 
                method ="lm", 
                na.action = na.pass)

anova(lmFit)

###
# Set up caret to perform 10-forld cross validation repeated 3 timed 
# and to use a grid search for optimal model hyperparamer.
###
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid")

###
# Leverage the grid search of hyperparameters for xgboost.
###
tune.grid <- expand.grid(eta = c(0.1),
                         nrounds = c(50, 75),
                         max_depth = 6:8,
                         min_child_weight = c(1.0, 2.25, 2.5),
                         colsample_bytes = c(0.3, 0.4, 0.5),
                         gamma = 0,
                         subsample = 1)
#View(tune.grid)

cl <- makeCluster(3, type = "SOCK")

# register cluster so that caret will know to train in parallel.
registerDoSNOW(cl)

# Train the xgboost model using 10-fold CV repeated 3 times
# and a hyperparameter frid search to train the optimal model.
caret.cv.XGB <- train( SalePrice ~ .,
                       data = dataset.train,
                       method = "xgbTree",
                       tunGrid = tune.grid,
                       trControl = train.control)
stopCluster(cl)

caret.cv.RF <- train( Survived ~ .,
                      data = titanic.train,
                      method = "rf",
                      tunGrid = tune.grid,
                      trControl = train.control)

caret.cv.xgbDART <- train( Survived ~ .,
                           data = titanic.train,
                           method = "xgbDART",
                           tunGrid = tune.grid,
                           trControl = train.control)

stopCluster(cl)



summary(lmFit)

###
# Cross validation
###
ctrl <- trainControl(method = 'cv',number = 10)

lmCVFit<-train(SalePrice ~ ., data = dataset, method = 'lm', trControl = ctrl, metric='Rsquared',na.action = na.pass)

summary(lmCVFit)

residuals<-resid(lmFit)

predictedValues<-predict(lmFit)

plot(df_train$SalePrice,residuals)

abline(0,0)

length(predictedValues)
length(dataset.validate$SalePrice)

plot(dataset.validate$SalePrice,predictedValues)
varImpPercent <- varImp(lmFit)
varImpPercent$importance <- varImpPercent$importance[varImpPercent$importance > 50]

summary(varImpPercent$importance)
str(varImpPercent$importance$Overall)
plot(varImpPercent)

predictedVal<-predict(lmFit,df_validate)

modelvalues<-data.frame(obs = df_validate$SalePrice, pred=predictedVal)

defaultSummary(modelvalues)

#############################################
