library(ggplot2)
library(stringr)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(RColorBrewer)


dfA2 <- dfAB[dfAB$status=='failed',]
dim(dfA2)

dfA2$region <- as.character(dfA2$region)
dfA2$'region'[dfA2$'region' == "Valle d'Aosta/Vallée d'Aoste"] <- "Valle d'Aosta"


#dfA2$y <- cut(dfA2$year, 4)


hist(dfA2$year,  main=str_c('Failed campnies by year'),
     xlab="Size",col="#03ab84", breaks =  "sturges")

dfA2$period = "1990 - 2010"
dfA2$period[dfA2$year>=2011] = "2011-2014"
dfA2$period[dfA2$year>=2015] = "2015-2020"
table(dfA2$period)



###################################
### SIZE GROUPED BARPLOT EXAPLE ###
###################################
barplot(prop.table(table(dfA2$period,dfA2$size), 1) , main=str_c('Failed "',form,'" by year'),
        xlab="Size", legend = rownames(table(dfA2$period,dfA2$size)), beside=TRUE)

barplot(prop.table(table(dfA2$year,dfA2$size), 1) , main=str_c('Failed "',form,'" by year'),
        xlab="Size",  beside=TRUE)


##APPLY THIS IN A FOR LOOP FOR ALL LEGAL FORMS
palette = brewer.pal(3,"Reds")
for (form in unique(dfA2$'Legal form')){
  jpeg(str_c("img/A/2/LegalForm/size/",form,".jpg"))
  df = dfA2[dfA2$`Legal form`==form,]
  barplot(prop.table(table(df$period,df$size), 1) , main=str_c('Failed "',form,'" by period'),
          xlab="Size", col = palette,
          legend = rownames(table(df$period,df$size)), beside=TRUE)
  
  dev.off()
}

##APPLY THIS IN A FOR LOOP FOR ALL REGIONS

palette = brewer.pal(3,"Oranges")
for (region in unique(dfA2$'region')){
  jpeg(str_c("img/A/2/Region/size/",region,".jpg"))
  df = dfA2[dfA2$`region`==region,]
  barplot(prop.table(table(df$y,df$size), 1) , main=str_c('Failed company in "',region,'" by period'),
          xlab="Size", col = palette,
          legend = rownames(table(df$y,df$size)), beside=TRUE)
  
  dev.off()
}



###################################
### AGE GROUPED BARPLOT EXAPLE ###
###################################
dfA2$y = dfA2$year
dfA2$year <- as.character(dfA2$year)

ggplot(dfA2, aes(x=age, fill=period)) + geom_density(alpha=0.4) #+
 # scale_fill_brewer(palette="Set1") 


ggplot(dfA2[dfA2$y>2013 & dfA2$y<2019,], aes(x=age, color=year)) + geom_density(size=1, alpha=0.4)




##APPLY THIS IN A FOR LOOP FOR ALL LEGAL FORMS


for (form in unique(dfA2$'Legal form')){
  
  df = dfA2[dfA2$`Legal form`==form,]
  
  ggplot(df, aes(x=age, fill=period)) + geom_density(alpha=0.2) +
    ggtitle(str_c('Failed "',form,'" by period'))
  ggsave(str_c("img/A/2/LegalForm/age/",form,"_period.jpg"),dpi=300)
  
  ggplot(df[df$y>2013 & df$y<2019,], aes(x=age, color=year)) + geom_density(size=0.5, alpha=0.4)+
    ggtitle(str_c('Failed "',form,'" in the last five years'))
  ggsave(str_c("img/A/2/LegalForm/age/",form,"_last5.jpg"),dpi=300)
  
  
  
}


##APPLY THIS IN A FOR ALL REGIONS

for (region in unique(dfA2$region)){
  
  df = dfA2[dfA2$region==region,]
  
  ggplot(df, aes(x=age, fill=period)) + geom_density(alpha=0.2) +
    ggtitle(str_c('Failed companies in "',region,'" by period'))
  ggsave(str_c("img/A/2/Region/age/",region,"_period.jpg"),dpi=300)
  
  ggplot(df[df$y>2013 & df$y<2019,], aes(x=age, color=year)) + geom_density(size=0.5, alpha=0.4)+
    ggtitle(str_c('Failed companies in "',region,'" in the last five years'))
  ggsave(str_c("img/A/2/Region/age/",region,"_last5.jpg"),dpi=300)
  
  
  
}
