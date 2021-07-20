dim(dfc)


library(dplyr)
library(sjmisc)
library(magrittr)
library(caret)
library(ggplot2)
library(devtools)
library(tseries)
library(moments)
library(stringr)
library(gbm)
library(MLmetrics)
library(ROCit)


###RANDOM FORES
trainrf.data = dfc[dfc$Incorporation.year < 2009,attributes]
testrf.data = dfc[dfc$Incorporation.year >= 2009,attributes]
# Estimate preprocessing parameters
preproc.param <- trainrf.data[,attributes] %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
trainrf.transformed <- preproc.param %>% predict(trainrf.data[,attributes])
testrf.transformed <- preproc.param %>% predict(testrf.data[,attributes])


#confusion matrix
predictions.rf <- predict(fit.rf, trainrf.transformed)
confusionMatrix(predictions.rf, as.factor(trainrf.transformed$`status`),positive="active")


crossMatrixSplit <- function(pred,obs){
  x <- c()
  for (i in c(1:length(pred))){
    if (pred[i] != obs[i]){
      x <- c(x,3)
    }
    else{
      x <- c(x,obs[i])
    }
  }
  return(x)}

x = crossMatrixSplit(predictions.rf,trainrf.transformed$status)

table(x)

trainrf.transformed$status <- as.factor(x)

table(trainrf.transformed$status)
fit.lda3 <- train(`status`~., data=trainrf.transformed, method="lda", metric="Kappa", trControl=control)

predictions.rf <- predict(fit.lda3, testrf.transformed)

length(predictions.rf[predictions.rf!='unknown'])
length(testrf.transformed[predictions.rf!='unknown',"status"])

library(comprehenr) 
x = to_vec(for (i in predictions.rf[predictions.rf!='unknown'] ) i)   
      
confusionMatrix(as.factor(x), as.factor(testrf.transformed[predictions.rf!='unknown',"status"]))


table(predictions.rf )
table(testrf.transformed$status)
testrf[predictions.rf!='unknown',]
