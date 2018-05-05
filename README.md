# R-ImarticusProject
---
output: html_document
---


# Set working directory

```{r, eval=TRUE}
setwd("D:/ujjwal/Tutorial/DataScience/Imarticus/Group Projects/R-ImarticusProject")

```


# Include the libraries required

```{r, eval=TRUE}
# install.packages("huxtable")
# install.packages("dplyr")
# install.packages('caret')
library(caret)
library(dplyr)
```

## Import other source codes (will be required to do some operations)

```{r, eval=TRUE}
source("cleanData.R")
```

```{r, eval=TRUE}
# Assign the data file name here.

DataFileName <- 'data\\Group Project on R-Data Set-4.csv'
# DataFileName <- 'data\\Gropup Project for R-Data Set.csv'

raw_data <- read.csv(DataFileName, stringsAsFactors = TRUE)
```

```{r, eval=FALSE}
# Make a copy of raw data 
dataset <- raw_data

# Let's see the data, how it looks
head(dataset)

# Summary of dataset will give us the picture how data is distributed.
summary(dataset)
```

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

