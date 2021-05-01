library(ggplot2)
library(stringr)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

dfA2 <- dfAB[dfAB$status=='failed',]
dim(dfA2)

dfA2$region <- as.character(dfA2$region)
dfA2$'region'[dfA2$'region' == "Valle d'Aosta/Vallée d'Aoste"] <- "Valle d'Aosta"


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

barplot(prop.table(table(dfA2$year,dfA2$size), 1) , main=str_c('Failed "',form,'" by year'),
        xlab="Size",  beside=TRUE)


##APPLY THIS IN A FOR LOOP FOR ALL LEGAL FORMS

for (form in unique(dfA1$'Legal form')){
  jpeg(str_c("img/A/2/LegalForm/size/",form,".jpg"))
  df = dfA2[dfA2$`Legal form`==form,]
  barplot(prop.table(table(df$y,df$size), 1) , main=str_c('Failed "',form,'" by period'),
          xlab="Size",
          legend = rownames(table(df$y,df$size)), beside=TRUE)
  
  dev.off()
}

##APPLY THIS IN A FOR LOOP FOR ALL REGIONS

for (region in unique(dfA2$'region')){
  jpeg(str_c("img/A/2/Region/size/",region,".jpg"))
  df = dfA2[dfA2$`region`==region,]
  barplot(prop.table(table(df$y,df$size), 1) , main=str_c('Failed company in "',region,'" by period'),
          xlab="Size",
          legend = rownames(table(df$y,df$size)), beside=TRUE)
  
  dev.off()
}



###################################
### AGE GROUPED BARPLOT EXAPLE ###
###################################
ggplot(dfA2, aes(x=age, fill=y)) + geom_density(alpha=0.4)

dfA2$yearchar <- as.character(dfA2$year)
ggplot(dfA2[dfA2$year>2013 & dfA2$year<2019,], aes(x=age, color=yearchar)) + geom_density(size=1, alpha=0.4)




##APPLY THIS IN A FOR LOOP FOR ALL LEGAL FORMS

for (form in unique(dfA2$'Legal form')){
  
  df = dfA2[dfA2$`Legal form`==form,]
  
  ggplot(dfA2, aes(x=age, fill=y)) + geom_density(alpha=0.4) +
    ggtitle(str_c('Failed "',form,'" by period'))
  ggsave(str_c("img/A/2/LegalForm/age/",form,"_period.jpg"))
  
  ggplot(dfA2[dfA2$year>2013 & dfA2$year<2019,], aes(x=age, color=yearchar)) + geom_density(size=1, alpha=0.4)+
    ggtitle(str_c('Failed "',form,'" in the last five years'))
  ggsave(str_c("img/A/2/LegalForm/age/",form,"_last5.jpg"))
  
  
  
}


##APPLY THIS IN A FOR ALL REGIONS

for (region in unique(dfA2$region)){
  
  df = dfA2[dfA2$region==region,]
  
  ggplot(dfA2, aes(x=age, fill=y)) + geom_density(alpha=0.4) +
    ggtitle(str_c('Failed company in "',region,'" by period'))
  ggsave(str_c("img/A/2/Region/age/",region,"_period.jpg"))
  
  ggplot(dfA2[dfA2$year>2013 & dfA2$year<2019,], aes(x=age, color=yearchar)) + geom_density(size=1, alpha=0.4)+
    ggtitle(str_c('Failed company in "',region,'" in the last five years'))
  ggsave(str_c("img/A/2/Region/age/",region,"_last5.jpg"))
  
  
  
}
