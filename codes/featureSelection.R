dfc <- read.csv(file = 'data/dfc200k.csv')




library(dplyr)
library(sjmisc)
library(magrittr)
library(caret)
library(ggplot2)
library(devtools)
library(tseries)
library(moments)
library(stringr)
library(LogicReg)
dim(dfc)
names(dfc)

for (colname in (names(dfc))){
  if (str_contains(colname,"Last.avail..yr")){
    names(dfc)[names(dfc) == colname] = substr(colname, 1, nchar(colname)-14)
  }}

names(dfc)    

dfc$status = as.factor(dfc$status)

attributes = c("Banks.turnover.", "Cash.Flowth.EUR","Cost.of.debit....." , "Current.liabilities.Tot.ass..",
               "Current.ratio"  , "Debt.EBITDA.ratio.","Debt.equity.ratio." , "EBITDA.Vendite."  ,  "EBITDAth.EUR" ,                  
               "Interest.Turnover....." , "Leverage"  , "Liquidity.ratio","Net.financial.positionth.EUR",   
               "Net.working.capitalth.EUR","Number.of.employees" ,"Profit..loss.th.EUR" ,"Return.on.asset..ROA..",
               "Return.on.equity..ROE.." ,"Return.on.investment..ROI......","Return.on.sales..ROS..",
               "Solvency.ratio....."  ,"Total.assets.turnover..times.","Total.assetsth.EUR","status")

attrs5kno = c("Banks.turnover.","Cost.of.debit....." ,"Return.on.investment..ROI......")

for (name in attributes[c(1:23)]){
  print(name)
  print(kurtosis(dfc[,name]))
  print(skewness(dfc[,name]))
  
  # ggplot(dfc, aes(x=dfc[,name])) + geom_density(alpha=0.4) +
  #   ggtitle(name)
  # 
  # 
  # ggsave(str_c("img/C/attributes/",name,".jpg"))
}


correlationMatrix <- cor(dfc[,2:23])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

library(reshape2)
library(scales)
melted_cormat <- melt(correlationMatrix,value.name='correlation')
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=correlation)) + 
  geom_tile()+theme(axis.text.x = element_text(angle = 40, hjust =1),axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_colour_gradient2( low = muted("red"),
                          mid = "white",
                          high = muted("blue"),
                          midpoint = 0,
                          space = "Lab",
                          na.value = "grey50",
                          guide = "colourbar",
                          aesthetics = "fill")
  ggsave("img/C/featureSelection/correlation.jpg",width=8.5,height = 7)


control <- trainControl(method="cv", number=10)
metric <- c("Accuracy","recall")
sample(1:nrow(dfc), 50000)

model <- train(status~., data=dfc[,attributes], method='lda', preProcess=(method = c("center", "scale")), trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


mostImportant = c("Cash.Flowth.EUR","Cost.of.debit.....", "Net.working.capitalth.EUR","EBITDAth.EUR" ,"Interest.Turnover....." , "Leverage"  ,"Solvency.ratio....."  ,"Return.on.equity..ROE.." )
length(mostImportant)

library(AppliedPredictiveModeling)
transparentTheme(trans = .4)

df = dfc[sample(1:nrow(dfc), 1000), ]
featurePlot(x = df[,mostImportant], 
            y = df$status, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 2))


for (name in attributes){
  if (name != 'status'){
    dfc[,name] = as.numeric( dfc[,name])
  }
  
}

library(MASS)
modellda <- lda(status ~ ., data=dfc[,c(mostImportant,'status')], prior=2, prior = c(0.85,0.15))

modellda

plot(modellda)


require(scales)
prop.lda = r$svd^2/sum(r$svd^2)
dataset = data.frame(status = dfc[,'status'], lda = modellda$x )
ggplot(dfc) + geom_point(aes(modellda.LD1, modellda.LD2, colour = tatus, shape = status), size = 2.5) 


### MULTICOLLINEARITA'####
library(mctest)
imcdiag(modellda)
library(GGally)
ggpairs(df[sample(1:nrow(dfc), 1000),mostImportant], legend=df$status)

dim(df)

