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

predictions.rf <- predict(fit.rf, testrf.transformed)

length(predictions.rf[predictions.rf!='unknown'])
length(testrf.transformed[predictions.rf!='unknown',"status"])

library(comprehenr) 
x = to_vec(for (i in predictions.rf[predictions.rf!='unknown'] ) i)   
      
confusionMatrix(as.factor(x), as.factor(testrf.transformed[predictions.rf!='unknown',"status"]))


table(predictions.rf )
table(trainrf.transformed$status)
#ROC curve
prob.rf <- predict(fit.rf, test.transformed, type ='prob')
ROCit_rf <- rocit(score=prob.rf,class=trainrf.transformed$status)
plot(ROCit_rf)


prob.rf <- predict(fit.rf, testrf.transformed, type ='prob')

table(is.na(prob.rf[trainrf.transformed$status=='active','active']))
plot(density(prob.rf[trainrf.transformed$status=='active','active']),xlim = c(0,1),col='green')
lines(density(prob.rf[trainrf.transformed$status=='failed','active']), type = "l",col='red')


prob.rf <- predict(fit.rf, testrf.transformed, type ='prob')
plot(density(prob.rf[testrf.transformed$status=='active','active']),xlim = c(0,1),col='green')
lines(density(prob.rf[testrf.transformed$status=='failed','active']), type = "l",col='red')

predictions.rf <- predict(fit.rf, testrf.transformed)

confusionMatrix(predictions.rf[prob.rf$active >=0.75 | prob.rf$active <=0.55], as.factor(testrf.transformed[prob.rf$active >=0.75 | prob.rf$active <=0.55,'status']),positive="active")

length(predictions.rf[prob.rf$active < 0.75 & prob.rf$active >0.55])

length(predictions.rf[prob.rf$active < 0.75 & prob.rf$active >0.55]) / length(predictions.rf)
