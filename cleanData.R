# R does not have a standard in-built function to calculate mode. So we 
# create a user function to calculate mode of a data set in R. This 
# function takes the vector as input and gives the mode value as output.

# Create a function to calculate and return the mode of values
getMode <- function(v){

  if(class(v) == "data.frame")
  {
    v <- v[, 1]
  }
  
  uniqv <- unique(v)
  if(class(v) == "integer"){
    return(which.max(tabulate(match(v, uniqv))))
  }
  else{
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
  }
  
}


processNA <- function(data, keepColNAPer){

  # Test values for runing the function code separately.
  data <- dataset
  keepColNAPer <- 0.4

  # Find out how many NA's we need to deal with
  sumNA <- sum(is.na(data))
  
  print(paste('Number of missing values in dataset : ', sumNA))
  print(paste('Current dimensions of the dataset : ', dim(data)[1], '(rows) ', dim(data)[2], '(columns)'))

    ## Return if the data is clean, i.e. no missing values
  if(sumNA == 0 ){
    return
  }
  
  ###
  # Select only those columns from dataset
  # for which the NA percentage is below the limit (keepColNAPer)
  ###

  # Option #1
  # ----------
  # colstokeep <- data %>% 
  #   summarize_all(funs((sum(is.na(.)) / length(.)) < keepColNAPer))
  # 
  # data <- data %>%
  #   select(names(colstokeep[, colstokeep == TRUE]))

  # Option #2
  # ----------
  colstokeep <- colMeans(is.na(data))

  # Select the required columns from given dataset
  data <- data %>%
    select(names(colstokeep[colstokeep < keepColNAPer]))
  
  print('Processing each columns with missing values')
  for(col in names(colstokeep[colstokeep > 0 & colstokeep < keepColNAPer])){
    # col <- 'MasVnrType'
    # col <- 'LotFrontage'
    print(paste('Column : ', col, ' (', sum(is.na(data[,col])) , ')', sep = ''))
    data[is.na(data[,col]), col] <- getMode(data[,col])
  }
  
  
  # Find out how many NA's we are left with
  sumNA <- sum(is.na(data))
  print(paste('Number of missing values in dataset : ', sumNA))
  print(paste('New dimensions of the dataset : ', dim(data)[1], '(rows) ', dim(data)[2], '(columns)'))
  
  return(data)
}

###
# Function to drop columns from dataset where % of missing values is more than
# the thresold.
#    dropColsNAPer = 0.4, will drop all the columns with more than 40% missing values
###
dropNAcols <- function(data, dropColsNAPer){
  
  # Test values for runing the function code separately.
  # data <- dataset
  # dropColsNAPer <- 0.4
  
  # Find out how many NA's we need to deal with
  sumNA <- sum(is.na(data))
  print(paste('Pre ==> Number of missing values in dataset : ', sumNA))
  print(paste('Pre ==> Dimensions of the dataset : ', dim(data)[1], '(rows) ', dim(data)[2], '(columns)'))
  
  ## Return if the data is clean, i.e. no missing values
  if(sumNA == 0 ){
    return
  }
  
  ###
  # Select only those columns from dataset
  # for which the NA percentage is below the limit (keepColNAPer)
  ###
  
  colstokeep <- colMeans(is.na(data))
  
  # Select the required columns from given dataset
  data <- data %>%
    select(names(colstokeep[colstokeep < dropColsNAPer]))
  
  # Find out how many NA's we are left with
  sumNA <- sum(is.na(data))
  print(paste('Post ==> Number of missing values in dataset : ', sumNA))
  print(paste('Post ==> Dimensions of the dataset : ', dim(data)[1], '(rows) ', dim(data)[2], '(columns)'))
  
  return(data)
}

check = function(pred, true_value){
  return(data.frame(RMSE = RMSE(pred[,1],true_value),
                    BIAS = BIAS(pred[,1],true_value),
                    maxDeviation = maxDeviation(pred[,1],true_value),
                    MeanAbsDeviation = MeanAbsDeviation(pred[,1],true_value),
                    Coverage = coverage(pred[,2], pred[,3], true_value)))
}
RMSE = function(y,pred) {
  rmse = sqrt(mean((y-pred)^2))
  return(rmse)
}

BIAS = function(pred, true_value){
  return(mean(pred-true_value))
}
maxDeviation = function(pred, true_value){
  return(max(abs(pred-true_value)))
}
MeanAbsDeviation = function(pred, true_value){
  return(mean(abs(pred-true_value)))
}
coverage = function(lwr,upr,true_value){
  mean(lwr<true_value & true_value<upr)
}