
processNA <- function(data){
  
  # data <- dataset

  nasum <- colSums(is.na(data)) > 0
  
  print('Columns with NA values : ')
  print(names(nasum[nasum > 0]))
  
  print('Columns with non-NA values : ')
  print(names(nasum[nasum == 0]))

}