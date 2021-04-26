library(ggplot2)
library(stringr)

dfA2 <- dfAB[dfAB$status=='failed',]
dim(dfA2)



#dfA2$y <- cut(dfA2$year, 4)

hist(dfA2$year, breaks =  "sturges")

dfA2$y = "1990 - 2010"
dfA2$y[dfA2$year>=2011] = "2011-2014"
dfA2$y[dfA2$year>=2015] = "2015-2020"
table(dfA2$y)
###################################
### SIZE GROUPED BARPLOT EXAPLE ###
###################################
barplot(prop.table(table(dfA2$y,dfA2$size), 1) , main=str_c('Failed "',form,'" by year'),
        xlab="Size", legend = rownames(table(dfA2$y,dfA2$size)), beside=TRUE)

