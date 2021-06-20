dfc <- read.csv(file = 'data/dfC200k.csv')




library(dplyr)
library(sjmisc)
library(magrittr)
library(caret)
library(ggplot2)
library(devtools)
library(tseries)
library(moments)
library(stringr)
install_github("Displayr/flipMultivariates")

dim(dfc)
names(dfc)

for (colname in (names(dfc))){
  if (str_contains(colname,"Last.avail..yr")){
    names(dfc)[names(dfc) == colname] = substr(colname, 1, nchar(colname)-14)
  }}

names(dfc)    

table(dfc$status)

activeCumulative =  dfAB[dfAB$status=='active',] %>% group_by(year,.add=TRUE) %>% summarise(n = n()) %>% mutate(n = cumsum(n))
activeCumulative$status = 'active'
failedCumulative = dfAB[dfc$status=='failed',] %>% group_by(year,.add=TRUE) %>% summarise(n = n()) %>% mutate(n = cumsum(n))
failedCumulative$status = 'failed'
cumulative  = data.frame(c(1:dim(failedCumulative)[1]+dim(activeCumulative)[1]), c('year','n','status'), activeCumulative)
cumulative = rbind(activeCumulative, failedCumulative)


ggplot(data = cumulative)+
       aes(x = year, y = n/dim(dfc)[1], fill=status )+
  scale_fill_manual(values=c("mediumseagreen","salmon3"))+   #"#04bc3c", "#fc746c")
  ylab("Percentageof members") +
  xlab("Date") +
  geom_bar(position="stack", stat="identity")





attributes = c("Banks.turnover.", "Cash.Flowth.EUR","Cost.of.debit....." , "Current.liabilities.Tot.ass..",
               "Current.ratio"  , "Debt.EBITDA.ratio.","Debt.equity.ratio." , "EBITDA.Vendite."  ,  "EBITDAth.EUR" ,                  
                "Interest.Turnover....." , "Leverage"  , "Liquidity.ratio","Net.financial.positionth.EUR",   
              "Net.working.capitalth.EUR","Number.of.employees" ,"Profit..loss.th.EUR" ,"Return.on.asset..ROA..",
              "Return.on.equity..ROE.." ,"Return.on.investment..ROI......","Return.on.sales..ROS..",
              "Solvency.ratio....."  ,"Total.assets.turnover..times.","Total.assetsth.EUR","status")


ggplot(dfAB, aes(x=dfc[,"Total.assets.turnover..times."])) + geom_density(alpha=0.4) +
  ggtitle("Total.assets.turnover..times.")





#from the plot hte only way to split the data temporlly is using >2018 as training, since 75% of data are from 2018
train.data = dfc[dfc$year < 2018,attributes]
test.data = dfc[dfc$year >= 2018,attributes]
table(train.data$active)





table(test.data$active)
# Estimate preprocessing parameters
preproc.param <- train.data[,attributes] %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data[,attributes])
test.transformed <- preproc.param %>% predict(test.data[,attributes])


control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


# a) linear algorithms
set.seed(17)
fit.lda <- train(`status`~., data=train.transformed, method="lda", metric=metric, trControl=control)

print(fit.lda)
predictions <- predict(fit.lda, test.transformed)
confusionMatrix(predictions, as.factor(test.transformed$`status`))
table(factor(predictions, levels=min(test.transformed):max(test.transformed)), 
      factor(test.transformed$status, levels=min(test.transformed):max(test.transformed)))

table(predictions)
table( test.transformed$`status`)

typeof(test.transformed$`status`).asfactor

       