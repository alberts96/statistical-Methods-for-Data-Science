dfc <- read.csv(file = 'data/dfC200k.csv')

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
library(classifierplots)
install_github("Displayr/flipMultivariates")

dim(dfc)
names(dfc)

for (colname in (names(dfc))){
  if (str_contains(colname,"Last.avail..yr")){
    names(dfc)[names(dfc) == colname] = substr(colname, 1, nchar(colname)-14)
  }}

names(dfc)    

table(dfc$status)

activeCumulative =  dfc[dfc$status=='active',] %>% group_by(Incorporation.year,.add=TRUE) %>% summarise(n = n()) %>% mutate(n = cumsum(n))
activeCumulative$status = 'active'
failedCumulative = dfc[dfc$status=='failed',] %>% group_by(Incorporation.year,.add=TRUE) %>% summarise(n = n()) %>% mutate(n = cumsum(n))
failedCumulative$status = 'failed'
cumulative  = data.frame(c(1:dim(failedCumulative)[1]+dim(activeCumulative)[1]), c('Incorporation.year','n','status'), activeCumulative)
cumulative = rbind(activeCumulative, failedCumulative)


ggplot(data = cumulative)+
       aes(x = Incorporation.year, y = n/dim(dfc)[1], fill=status )+
  scale_fill_manual(values=c("mediumseagreen","salmon3"))+   #"#04bc3c", "#fc746c")
  ylab("Percentageof members") +
  xlab("Date") + xlim(1930,2020)+
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
train.data = dfc[dfc$Incorporation.year < 2009,c(mostImportant,"status")]
test.data = dfc[dfc$Incorporation.year >= 2009,c(mostImportant,"status")]
nrow(train.data) / nrow(dfc)
table(train.data$status) / nrow(train.data)
table(test.data$status)/ nrow(test.data)


sample(1:nrow(dfc), 50000)




table(predictions)
table( test.data$`status`)

typeof(test.transformed$`status`).asfactor


# Estimate preprocessing parameters
preproc.param <- train.data[,c(mostImportant,"status")] %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data[,c(mostImportant,"status")])
test.transformed <- preproc.param %>% predict(test.data[,c(mostImportant,"status")])



######DEFIN E CUSTOM METRICS
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  c(F1 = f1_val)
}

spec <- function(data, lev = NULL, model = NULL) {
    spec_val <- MLmetrics::Specificity(y_pred = data$pred, y_true = data$obs, positive = "1")
    c(SPEC = spec_val)
}


control <- trainControl(method="cv", number=10,classProbs = TRUE)

metric <- "Kappa"

# Create model weights (they sum to one)
model_weights <- ifelse(train.data$status == "active",
                        (1/table(train.data$status)[1]) * 0.5,
                        (1/table(train.data$status)[2]) * 0.5)


# a) linear algorithms
set.seed(17)
fit.lda <- train(`status`~., data=train.transformed, method="lda", metric=metric, trControl=control,weights = model_weights)

print(fit.lda)

# confusion matrix
predictions.lda <- predict(fit.lda, test.transformed)
confusionMatrix(predictions.lda, as.factor(test.transformed$`status`),positive="active")

#ROC curve
prob.lda <- predict(fit.lda, test.transformed, type ='prob')
ROCit_lda<- rocit(score=prob.lda,class=test.transformed$status)
ROCit_ldaW<- rocit(score=prob.lda$failed,class=test.transformed$status)

classifierplots(ifelse(test.transformed$status == 'active',1,0),prob.lda$active)
plot(ROCit_lda, col = c(5,'grey50'),legend=FALSE)
lines(ROCit_ldaW$TPR~ROCit_ldaW$FPR, col=6)
legend("bottomright", col = c(75,6), c("LDA", "Weighted LDA"), lwd = 2)

#Calibration
calibrate.plot(test.transformed$status, prob.lda$failed)

#specificity
myspecificity(predictions.lda,test.transformed$status,positive = "active")


# b) LOGISTIC REGRESSION 
calibration(active~failed,data = prob.lda)
set.seed(17)
fit.lr <- train(`status`~., data=train.transformed, method="LogitBoost", metric="Kappa", trControl=control, weights = model_weights)

print(fit.lr)

#CONFUSION MATRIX
predictions.lr <- predict(fit.lr, test.transformed)
confusionMatrix(predictions.lr, as.factor(test.transformed$`status`),positive="active")

#ROC curves
prob.lr <- predict(fit.lr, test.transformed, type ='prob')
ROCit_lr <- rocit(score=prob.lr$failed,class=test.transformed$status)
plot(ROCit_lr)
ROCit_lrW <- rocit(score=prob.lr$failed,class=test.transformed$status)
plot(ROCit_lr, col = c(5,'grey50'),legend=FALSE)
lines(ROCit_lrW$TPR~ROCit_lrW$FPR, col=6)
legend("bottomright", col = c(5,6), c("Logit Boost", "Weighted Logit Boost"), lwd = 2)
classifierplots(ifelse(test.transformed$status == 'active',1,0),prob.lr$active)

###RANDOM FORES
trainrf.data = dfc[dfc$Incorporation.year < 2009,attributes]
testrf.data = dfc[dfc$Incorporation.year >= 2009,attributes]
# Estimate preprocessing parameters
preproc.param <- trainrf.data[,attributes] %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
trainrf.transformed <- preproc.param %>% predict(trainrf.data[,attributes])
testrf.transformed <- preproc.param %>% predict(testrf.data[,attributes])

fit.rf <- train(`status`~., data=trainrf.transformed, method="rf", metric="Kappa", trControl=control, weights = model_weights)

print(fit.rf)

#confusion matrix
predictions.rf <- predict(fit.rf, testrf.transformed)
confusionMatrix(predictions.rf, as.factor(testrf.transformed$`status`),positive="active")

#ROC curve
prob.rf <- predict(fit.rf, testrf.transformed, type ='prob')
ROCit_rf <- rocit(score=prob.rf,class=testrf.transformed$status)
plot(ROCit_rf)

#Calibration
calibrate.plot(testrf.transformed$status, prob.rf$failed)

classifierplots(ifelse(testrf.transformed$status == 'active',0,1),prob.rf$failed)



## All ROC curves
plot(ROCit_lda, col=c(2,"grey50"),legend = FALSE, YIndex = FALSE)
lines(ROCit_lr$TPR~ROCit_lr$FPR, col=3,lw=2)
lines(ROCit_rf$TPR~ROCit_rf$FPR, col=4,lw=2)

legend("bottomright", col = c(2,3,4), c("LDA","Logit Boost", "Ranfom Forest"), lwd = 2)


#All calibrations


#Calibration


testProbs <- data.frame(obs = test.transformed$status, lda = prob.lda$active, lr = prob.lr$active,rf = prob.rf$active)#, lr = prob.lr$active)
calPlotData <- calibration(obs ~ lda + lr + rf, data = testProbs)
xyplot(calPlotData, auto.key = list(columns = 3))




