###########
# AIDA data loading
###########

# clean all
rm(list=ls())



install.packages("tidyverse")
# to reload the whole dataset at once, run
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

load(file="data/aida.RData")
View(aida)

# summaries
table(aida$`Registered office address - Region`)
table(aida$`Legal status`)
table(aida$'Legal form')
#table(aida$`Company name`)

aida.column_list
lapply(aida,function(x) { length(which(is.na(x)))})
sum(is.na(aida))
