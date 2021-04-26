
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




 ###################################
 ### SIZE GROUPED BARPLOT EXAPLE ###
 ###################################
barplot(prop.table(table(dfA1$status,dfA1$size), 1) , main=str_c('Active and failed for "',form,'" in ',year),
        xlab="Size", col=c("green3","red3"),
        legend = rownames(table(dfA1$status,dfA1$size)), beside=TRUE)


##APPLY THIS IN A FOR LOOP FOR ALL LEGAL FORMS

for (form in unique(dfA1$'Legal form')){
  jpeg(str_c("img/A/1/LegalForm/size/",form,".jpg"))
  df = dfA1[dfA1$`Legal form`==form,]
  barplot(prop.table(table(df$status,df$size), 1) , main=str_c('Active and failed for "',form,'" in ',year),
          xlab="Size", col=c("green3","red3"),
          legend = rownames(table(df$status,df$size)), beside=TRUE)
  
  dev.off()
}


##APPLY THIS IN A FOR LOOP FOR ALL ATECO CODES

for (ateco in unique(dfA1$ATECO10)){
  jpeg(str_c("img/A/1/ATECO/size/",str_sub(ateco, 1, 3),".jpg"))
  df = dfA1[dfA1$`ATECO10`==ateco,]
  barplot(prop.table(table(df$status,df$size), 1) , main=str_c('Active and failed for "',ateco,'" in ',year),
          xlab="Size", col=c("green3","red3"),
          legend = rownames(table(df$status,df$size)), beside=TRUE)
  
  dev.off()
}



###################################
### Distribution for age  ###
###################################

p<-ggplot(dfA1, aes(x=age, fill=status)) + geom_density(alpha=0.4) + scale_fill_manual(values=c("#04bc3c", "#fc746c"))
p


##APPLY THIS IN A FOR LOOP FOR ALL LEGAL FORMS

for (form in unique(dfA1$'Legal form')){
  
  df = dfA1[dfA1$`Legal form`==form,]
  
  ggplot(df, aes(x=age, fill=status)) + geom_density(alpha=0.4) +
    ggtitle(str_c('Active and failed for "',form,'" in ',year)) +
    scale_fill_manual(values=c("#04bc3c", "#fc746c"))
  
  ggsave(str_c("img/A/1/LegalForm/age/",form,".jpg"))
}


##APPLY THIS IN A FOR LOOP FOR ALL ATECO CODES

for (ateco in unique(dfA1$ATECO10)){
  
  df = dfA1[dfA1$`ATECO10`== ateco,]
  
  ggplot(df, aes(x=age, fill=status)) + geom_density(alpha=0.4) +
    ggtitle(str_c('Active and failed for "',ateco,'" in ',year))+ 
    scale_fill_manual(values=c("#04bc3c", "#fc746c"))
  
  ggsave(str_c("img/A/1/ATECO/age/",str_sub(ateco, 1, 3),".jpg"))
}

