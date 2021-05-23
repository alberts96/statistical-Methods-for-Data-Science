library(tidyverse)

aida

summary(aida)


aida['Project Status']= "Failed"
aida$`Project Status`[aida$`Legal status`== "Active"] = "Active"
aida$`Project Status`[aida$`Legal status`== "Active (default of payments)"] = "Active"
aida$`Project Status`[aida$`Legal status`== "Active (receivership)"] = "Active"


datasetC = aida[, c("Project Status", "Return on investment (ROI) (%)%Last avail. yr", "Return on sales (ROS)%Last avail. yr", "Return on equity (ROE)%Last avail. yr", "EBITDA/Vendite%Last avail. yr", "Net financial positionth EURLast avail. yr", "Net working capitalth EURLast avail. yr", "Total assets turnover (times)Last avail. yr")]


datasetC_no_missing = datasetC[complete.cases(datasetC),] #remove missing values that were still in the dataset


names(datasetC_no_missing)[names(datasetC_no_missing) == "Return on investment (ROI) (%)%Last avail. yr"] <- "ROI"
names(datasetC_no_missing)[names(datasetC_no_missing) == "Return on sales (ROS)%Last avail. yr"] <- "ROS"
names(datasetC_no_missing)[names(datasetC_no_missing) == "Return on equity (ROE)%Last avail. yr"] <- "ROE"
names(datasetC_no_missing)[names(datasetC_no_missing) == "EBITDA/Vendite%Last avail. yr"] <- "EBITDA_over_Vendite"
names(datasetC_no_missing)[names(datasetC_no_missing) == "Net financial positionth EURLast avail. yr"] <- "NetFinancialPos"
names(datasetC_no_missing)[names(datasetC_no_missing) == "Net working capitalth EURLast avail. yr"] <- "NetWorkingCap"
names(datasetC_no_missing)[names(datasetC_no_missing) == "Total assets turnover (times)Last avail. yr"] <- "TotalAssets"

install.packages('e1071', dependencies=TRUE)

library(magrittr)
library(caret)



# Split the data into training (80%) and test set (20%)

set.seed(123)
training.samples <- datasetC_no_missing$`Project Status` %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- datasetC_no_missing[training.samples, ]
test.data <- datasetC_no_missing[-training.samples, ]


# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))          #method = "center" subtracts the mean of the predictor's data (again from the data in x) from the predictor values while method = "scale" divides by the standard deviation. 
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)      #è una pipeline?



control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(`Project Status`~., data=train.transformed, method="lda", metric=metric, trControl=control)  # `Project Status`~. <--- variabile oggetto?

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(`Project Status`~., data=train.transformed, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(`Project Status`~., data=train.transformed, method="knn", metric=metric, trControl=control)
# c) advanced algorithms





# summarize accuracy of models 
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn))   #cosa fa esattamente resamples? perchè non faccio la stessa cosa ma senza resamples?
summary(results)



# compare accuracy of models
dotplot(results)     # kappa = It is a score of how much homogeneity or consensus exists in the ratings given by various judges.

# summarize Models
print(fit.cart)
print(fit.lda)
print(fit.knn)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, test.transformed)       # mi
confusionMatrix(predictions, test.transformed$`Project Status`)


# estimate skill of CART on the validation dataset
predictions <- predict(fit.cart, test.transformed)
confusionMatrix(predictions, test.transformed$`Project Status`)

# estimate skill of KNN on the validation dataset
predictions <- predict(fit.knn, test.transformed)
confusionMatrix(predictions, test.transformed$`Project Status`)


