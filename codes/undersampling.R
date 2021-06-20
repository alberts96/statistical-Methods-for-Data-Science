dfc <- read.csv(file = 'data/dfC500k.csv')

library(dplyr)
library(sjmisc)
library(magrittr)
library(caret)


dim(dfc)
names(dfc)

for (colname in (names(dfc))){
  if (str_contains(colname,"Last.avail..yr")){
    names(dfc)[names(dfc) == colname] = substr(colname, 1, nchar(colname)-14)
  }}
      
names(dfc)    

table(dfc$status)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%&& TRAIN TEST SPLIT   %%%%%%%%%
testindex <- dfc$active %>%
  createDataPartition(p = 6/44, list = FALSE)

train <- dfc[-testindex, ]
test <- dfc[testindex, ]

#%%%% UNDERSAMPLE THE TRAINING SET %%%%

  
names(dfc)
  
  




