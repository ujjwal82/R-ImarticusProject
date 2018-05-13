
###
# Set working directory
###
setwd("D:/ujjwal/Tutorial/DataScience/Imarticus/Group Projects/R-ImarticusProject")

# install.packages("dplyr")
# install.packages('caret')
# install.packages('doSNOW')
# install.packages('h2o')
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(caret))
suppressMessages(library(gmodels))
suppressMessages(library(h2o))

suppressMessages(library(doSNOW))

suppressMessages(library(randomForest))
suppressMessages(library(gbm))
suppressMessages(library(corrplot))
suppressMessages(library(knitr))
suppressMessages(library(glmnet))


source("cleanData.R")

# Assign the data file name here.

TrainDataFileName <- 'data\\Group Project on R-Data Set-4.csv'
# TrainDataFileName <- 'data\\Gropup Project for R-Data Set.csv'

TestFileName <- 'data\\Group Project on R-Data Set-3.csv'

raw_traindata <- read.csv(TrainDataFileName, stringsAsFactors = TRUE)
raw_testdata <- read.csv(TestFileName, stringsAsFactors = TRUE)



train = raw_traindata %>% 
  mutate(datset = "train")
test = raw_testdata %>%
  mutate(SalePrice = mean(train$SalePrice), 
         datset = "test")

# Make a copy of raw data 
dataset <- train %>%
  rbind(., test)

# Let's see the data, how it looks
#head(dataset)

# Summary of dataset will give us the picture how data is distributed.
#summary(dataset)

# List down all factors in the dataset
# names(Filter(is.numeric, dataset))

###
# Step - Data cleaning
###

# Drop columns with more than 40% of missing values
dataset <- dropNAcols(dataset, 0.4)


# Find out how many NA's we need to deal with
sumNA <- sum(is.na(dataset))
print(paste('Pre ==> Number of missing values in dataset : ', sumNA))
print(paste('Pre ==> Dimensions of the dataset : ', dim(dataset)[1], '(rows) ', dim(dataset)[2], '(columns)'))

###
# for all the categorical variables, create new level 'NA' 
# for missing values.
# And then convert into integer values.
###
for( colName in names(Filter(is.factor, dataset))){
  print(paste('Processing column : ', colName))

  dataset[, colName] <- fct_explicit_na(dataset[, colName], 'NA')
  # dataset[, colName] <- as.numeric(factor(dataset[, colName],
  #                         levels = levels(dataset[, colName])))
}

# Find out how many NA's we need to deal with
sumNA <- sum(is.na(dataset))
print(paste('Pre ==> Number of missing values in dataset : ', sumNA))
print(paste('Pre ==> Dimensions of the dataset : ', dim(dataset)[1], '(rows) ', dim(dataset)[2], '(columns)'))


colstokeep <- colMeans(is.na(dataset))

for( colName in names(colstokeep[colstokeep > 0])){
  print(paste('Processing column : ', colName))
  
  dataset[, colName] <- ifelse(is.na(dataset[, colName]), 0, dataset[, colName])
}

# Find out how many NA's we need to deal with
sumNA <- sum(is.na(dataset))
print(paste('Pre ==> Number of missing values in dataset : ', sumNA))
print(paste('Pre ==> Dimensions of the dataset : ', dim(dataset)[1], '(rows) ', dim(dataset)[2], '(columns)'))

plot(x = dataset$SalePrice, type = "o")
hist(dataset$SalePrice)



# Let's separate out the train and test data, since we are done with the cleanup activities.

train <- dataset %>%
  filter(datset == "train") %>%
  select(-datset)

test <- dataset %>%
  filter(datset == "test") %>%
  select(-datset, -SalePrice)


colnames(train)
ggplot(train, aes(HouseStyle, fill=GarageType))+
  geom_bar()


## Cross table is used to compare two categorical variables in dataset.
CrossTable(train$HouseStyle, train$GarageType)


split<-createDataPartition(y = train$SalePrice, 
                           # times = 1, 
                           p = 0.9,
                           list = FALSE)
dataset.train <- train[ split, ]
dataset.validate <- train[ -split, ]

summary(dataset.train$RoofMatl)



# initialize the H2O
localh2o <- h2o.init(nthreads = -1)

# Data to h2o cluster
train.h2o <- as.h2o(dataset.train)
test.h2o <- as.h2o(dataset.validate)

colnames(train.h2o)

# dependent variable
y.dep <- 76

# independent variables( dropping ID and dependent variable )
x.indep <- c(2:75)


###
# Let's start multiple regression models.
###

# H2O: glm
regression.model <- h2o.glm( y = y.dep, x = x.indep, 
                             training_frame = train.h2o, 
                             nfolds = 3,
                             family = "gaussian")

h2o.performance(regression.model)

predict.glm <- as.data.frame(h2o.predict(regression.model, test.h2o))
sub_reg <- data.frame(Actuals = dataset.validate$SalePrice, 
                      'GLM' = predict.glm$predict)

# H2O: random forest
system.time(
  rforest.model <- h2o.randomForest(y = y.dep, x = x.indep, 
                                    training_frame = train.h2o,
                                    ntrees = 1000, mtries = 3, nfolds = 3,
                                    max_depth = 4, seed =  12345)
)
h2o.performance(rforest.model)

predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
sub_reg <- cbind(sub_reg, 'RFOREST' = predict.rforest$predict)

# H2O: GBM
system.time(
gbm.model <- h2o.gbm(y = y.dep, x = x.indep, 
                     training_frame = train.h2o,
                     ntrees = 1000, max_depth = 4, nfolds = 3,
                     learn_rate = 0.01, seed =  12345)
)

h2o.performance(gbm.model)

predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))
sub_reg <- cbind(sub_reg, 'GBM' = predict.gbm$predict)


# H2O: Deep learning
system.time(
  dlearning.model <- h2o.deeplearning(
    y = y.dep, x = x.indep,
    training_frame = train.h2o,
    epoch = 60,
    hidden = c(100, 100),
    activation =  'Rectifier',
    nfolds = 3,
    seed = 12345
  )
)


h2o.performance(dlearning.model)
predict.dlearning <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
sub_reg <- cbind(sub_reg, 'GBM' = dlearning.model$predict)


h2o.performance(regression.model)
h2o.performance(rforest.model)
h2o.performance(gbm.model)
h2o.performance(dlearning.model)




# Check the variable importance
h2o.varimp(regression.model)
h2o.varimp(rforest.model)
dlearning.varimp <- h2o.varimp(dlearning.model)



###
# Create linear model
###
lm_model <- lm(formula = SalePrice ~ ., data = dataset.train)
summary(lm_model)


lmFit <- train( SalePrice ~ ., 
                data = dataset.train, 
                method ="lm", 
                na.action = na.pass)

warnings()
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
