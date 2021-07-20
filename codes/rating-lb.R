dim(dfc)


library(dplyr)
library(sjmisc)
library(magrittr)r

library(caret)
library(ggplot2)
library(devtools)
library(tseries)
library(moments)
library(stringr)
library(gbm)
library(MLmetrics)
library(ROCit)




train.data = dfc[dfc$Incorporation.year < 2012,c(mostImportant,"status")]
test.data = dfc[dfc$Incorporation.year >= 2012,c(mostImportant,"status")]
nrow(train.data) / nrow(dfc)
table(train.data$status) / nrow(train.data)
table(test.data$status)/ nrow(test.data)
train.active = train.data[train.data$status=='active',]
train.failed = train.data[train.data$status=='failed',]
train.active = train.active[sample(1:nrow(train.active), nrow(train.failed)*2),]
train.data <- rbind(train.failed,train.active)

nrow(train.data) / (nrow(train.data)+nrow(test.data))
table(train.data$status) 
table(test.data$status)/ nrow(test.data)

# Estimate preprocessing parameters
preproc.param <- train.data[,c(mostImportant,"status")] %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data[,c(mostImportant,"status")])
test.transformed <- preproc.param %>% predict(test.data[,c(mostImportant,"status")])


table( test.data$`status`)



#CONFUSION MATRIX
predictions.lr <- predict(fit.lr, train.transformed)
confusionMatrix(predictions.lr, as.factor(train.transformed$`status`),positive="active")



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

x = crossMatrixSplit(predictions.lr,train.transformed$status)


table(x)

train.transformed$status = x


fit.lr3 <- train(`status`~., data=train.transformed, method="LogitBoost", metric="Kappa", trControl=control,weights = model_weights)

