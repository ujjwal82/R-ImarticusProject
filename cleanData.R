# R does not have a standard in-built function to calculate mode. So we 
# create a user function to calculate mode of a data set in R. This 
# function takes the vector as input and gives the mode value as output.

# Create a function to calculate and return the mode of values
getMode <- function(v){
  # v <- data[, col]
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

  data <- dataset
  keepColNAPer <- 0.4

  # Find out how many NA's we need to deal with
  sumNA <- sum(is.na(data))
  
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
  # dim(data)
  
  sum(is.na(data))
  dim(data)
  

  for(col in names(colstokeep[colstokeep > 0 & colstokeep < keepColNAPer])){
    # col <- 'MasVnrType'
    # col <- 'LotFrontage'
    print(paste('Processing column : ', col))
    data[is.na(data[,col]), col] <- getMode(data[,col])
  }
  
  print(sum(is.na(data)))
  print(dim(data))
  
  return(data)
}

