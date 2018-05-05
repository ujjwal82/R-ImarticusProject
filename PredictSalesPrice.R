
###
# Set working directory
###
setwd("D:/ujjwal/Tutorial/DataScience/Imarticus/Group Projects/R/Work")

# install.packages("huxtable")
# install.packages("dplyr")
# install.packages('caret')
library(caret)
library(dplyr)

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

# Let's list down all factors in the dataset
names(Filter(is.factor, dataset))

# List down all factors in the dataset
names(Filter(is.numeric, dataset))

# Find out how many NA's we need to deal with
sumNA <- sum(is.na(dataset))
# we have single NA

# Now we need to find which column has this single NA value

if(sumNA > 1){
  processNA(dataset)
}

split<-createDataPartition(y = dataset$SalePrice, p = 0.85,list = FALSE)
df_train <- dataset[ split, ]
df_validate <- dataset[ -split, ]

###
# Create linear model
###
lmFit <- train( SalePrice ~ ., data = df_train, method ="rf", na.action = na.pass)
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

plot(dev$mpg,predictedValues)
varImp(lmFit)

plot(varImp(lmFit))
predictedVal<-predict(lmFit,df_validate)

modelvalues<-data.frame(obs = df_validate$SalePrice, pred=predictedVal)

defaultSummary(modelvalues)

#############################################
train_control <- trainControl(method="boot", number=100)
library(klaR)
library(naivebayes)
model <- train(SalePrice~., data=data, trControl=train_control, method="nb", na.action = na.pass)
# summarize results
print(model)