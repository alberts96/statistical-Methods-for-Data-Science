
install.packages("pacman")
pacman::p_load( pacman, sm, dplyr, hrbrthemes, GGally, ggplot2, ggthemes,
               ggvis, httr, lubridate, pylotly, rio, rmarkdown, shiny,
               stringr, tidyr)
pacman::p_load(  sm)
library(ggplot2)
library(stringr)
table(df$year)
year = 2018 #2018 is the most populet year
dfA1 <- dfAB[dfAB$year==year,]
dim(dfA1)
names(dfA1)



 ###################################
 ### SIZE GROUPED BARPLOT EXAPLE ###
 ###################################

barplot(prop.table(table(dfA1$status,dfA1$size), 1) , main=str_c('Active and failed companies for in ',year),
        xlab="Size", col=c("mediumseagreen","salmon3"),
        legend = rownames(table(dfA1$status,dfA1$size)), beside=TRUE)




##APPLY THIS IN A FOR LOOP FOR ALL LEGAL FORMS


test  = ks.test(df$size[df$status=='failed'],df$size[df$status=='active'])

tests = data.frame(row.names = c('D','p-value'))
for (form in unique(dfA1$'Legal form')){
  jpeg(str_c("img/A/1/LegalForm/size/",form,".jpg"))
   df = dfA1[dfA1$`Legal form`==form,]
  barplot(prop.table(table(df$status,df$size), 1) , main=str_c('Active and failed for "',form,'" in ',year),
          xlab="Size", col=c("mediumseagreen","salmon3"),
          legend = rownames(table(df$status,df$size)), beside=TRUE)

  dev.off()
  print(ks.test(df[df$status=='active','size'],df$'size'))
  print(ks.test(df[df$status=='failed','size'],df$'size'))
  test = ks.test(df$size[df$status=='active'],df$size[df$status=='failed'])
  tests[form] = c(test$statistic,test$p.value)
}
write.csv(tests,'img/A/1/A1legalformSize.csv')

##APPLY THIS IN A FOR LOOP FOR ALL ATECO CODES

tests = data.frame(row.names = c('D','p-value'))
for (ateco in unique(dfA1$ATECO)){
  jpeg(str_c("img/A/1/ATECO/size/",str_sub(ateco, 1, 3),".jpg"))
  df = dfA1[dfA1$`ATECO`==ateco,]
  barplot(prop.table(table(df$status,df$size), 1) , main=str_c('Active and failed for "',ateco,'" in ',year),
          xlab="Size", col=c("mediumseagreen","salmon3"),
          legend = rownames(table(df$status,df$size)), beside=TRUE)
  
  dev.off()
  test = ks.test(df$size[df$status=='active'],df$size[df$status=='failed'])
  tests[ateco] = c(test$statistic,test$p.value)
}
write.csv(tests,'img/A/1/A1atecoSize.csv')
table(dfA1$ATECO)


###################################
### Distribution for age  ###
###################################

ggplot(dfA1, aes(x=age, fill=status)) + geom_density(alpha=0.4) + scale_fill_manual(values=c("#04bc3c", "#fc746c"))



##APPLY THIS IN A FOR LOOP FOR ALL LEGAL FORMS
tests = data.frame(row.names = c('D','p-value'))
for (form in unique(dfA1$'Legal form')){
#   
   df = dfA1[dfA1$`Legal form`==form,]

  ggplot(df, aes(x=age, fill=status)) + geom_density(alpha=0.4) +
    ggtitle(str_c('Active and failed for "',form,'" in ',year)) +
    scale_fill_manual(values=c("#04bc3c", "#fc746c"))

  ggsave(str_c("img/A/1/LegalForm/age/",form,".jpg"))
  test = ks.test(df$age[df$status=='active'],df$age[df$status=='failed'])
  tests[form] = c(test$statistic,test$p.value)
}
write.csv(tests,'img/A/1/A1legalformAge.csv')
table(dfA1$ATECO)



##APPLY THIS IN A FOR LOOP FOR ALL ATECO CODES
tests = data.frame(row.names = c('D','p-value'))
for (ateco in unique(dfA1$ATECO)){
  
  df = dfA1[dfA1$`ATECO`== ateco,]
  
  ggplot(df, aes(x=age, fill=status)) + geom_density(alpha=0.4) +
    ggtitle(str_c('Active and failed for "',ateco,'" in ',year))+
    scale_fill_manual(values=c("#04bc3c", "#fc746c"))

  ggsave(str_c("img/A/1/ATECO/age/",str_sub(ateco, 1, 3),".jpg"))
  test = ks.test(df$age[df$status=='active'],df$age[df$status=='failed'])
  tests[ateco] = c(test$statistic,test$p.value)
}
write.csv(tests,'img/A/1/A1atecoAge.csv')




###################################
### Distribution for anet working capital  ###
###################################

ggplot(dfA1, aes(x=capital, fill=status)) + geom_density(alpha=0.4) + xlim(-100,100)+ scale_fill_manual(values=c("#04bc3c", "#fc746c"))



##APPLY THIS IN A FOR LOOP FOR ALL LEGAL FORMS
tests = data.frame(row.names = c('D','p-value'))
for (form in unique(dfA1$'Legal form')){
  
  df = dfA1[dfA1$`Legal form`==form,]

  ggplot(df, aes(x=capital, fill=status)) + geom_density(alpha=0.4) + xlim(-100,100) +
    ggtitle(str_c('Active and failed for "',form,'" in ',year)) +
    scale_fill_manual(values=c("#04bc3c", "#fc746c"))

  ggsave(str_c("img/A/1/LegalForm/capital/",form,".jpg"))
  test = ks.test(df$capital[df$status=='active'],df$capital[df$status=='failed'])
  tests[form] = c(test$statistic,test$p.value)
}
write.csv(tests,'img/A/1/A1legalformCapital.csv')




##APPLY THIS IN A FOR LOOP FOR ALL ATECO CODES

tests = data.frame(row.names = c('D','p-value'))
for (ateco in unique(dfA1$ATECO)){
  
  df = dfA1[dfA1$`ATECO`== ateco,]
# 
#   ggplot(df, aes(x=capital, fill=status)) + geom_density(alpha=0.4) + xlim(-100,100)+
#     ggtitle(str_c('Active and failed for "',ateco,'" in ',year))+
#     scale_fill_manual(values=c("#04bc3c", "#fc746c"))
# 
#   ggsave(str_c("img/A/1/ATECO/capital/",str_sub(ateco, 1, 3),".jpg"))
  test = ks.test(df$capital[df$status=='active'],df$capital[df$status=='failed'])
  tests[ateco] = c(test$statistic,test$p.value)
}
write.csv(tests,'img/A/1/A1atecoCapital.csv')

