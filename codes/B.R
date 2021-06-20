library(ggplot2)
library(stringr)
library(tidyverse)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(RColorBrewer)
####PREPARATION#####

dfb <- dfAB[dfAB$status=='failed',]


dfb$region <- as.character(dfb$region)
dfb$'region'[dfb$'region' == "Valle d'Aosta/Vallée d'Aoste"] <- "Valle d'Aosta"

year = 2018 #2018 is the most populet year
dfb <- dfb[dfb$year==year,]
dim(dfb)


table(dfb$`ATECO10`)



#################################
########### LEGAL FORM ###########

dfb$`Legal form`[dfb$`Legal form` == "S.N.C." | dfb$`Legal form` == "S.A.S." | dfb$`Legal form` == "Social cooperative company"] = "Other"

dfb$legalform = NaN
for (lf in unique(dfb$`Legal form`)){
  dfb$`legalform`[dfb$`Legal form` == lf] = lf
}
table(dfb$`legalform`)


##SIZE 
palette = brewer.pal(length(unique(dfb$legalform)),"Dark2")
barplot(prop.table(table(dfb$`legalform`,dfb$size), 1) , main='Failed by year and legal form',
        xlab="Size",col=palette,  beside=TRUE)
barplot(prop.table(table(dfb$`legalform`,dfb$size), 1) , main='Failed by year and legal form',
        xlab="",col=palette,legend=TRUE,  beside=TRUE)

legend(1, 95, col=palette,legend=c(dfb$`Legal form`),
       lty=1:2, cex=0.8)


#violinplots
dfb$legalform = with(dfb, reorder(legalform, size, var))

dfb %>%
  ggplot( aes(x=legalform, size, fill=legalform)) + 
  geom_violin() +
  xlab("Legal Form") +
  theme(legend.position="none") +
  xlab("")
#AGE

ggplot(dfb, aes(x=age, color=legalform, legend=FALSE)) + geom_density(size=0.5, alpha=0.4)+
  ggtitle(str_c('Failed companies by  legal form')) 
  #(palette='Dark2')



#boxplots
dfb %>%
  mutate(class = fct_reorder(legalform,age, .fun='mean' )) %>%
  ggplot( aes(x=reorder(legalform,age), y=age, fill=class)) + 
  geom_boxplot() +
  xlab("Legal Form") +
  theme(legend.position="none") +
  xlab("") +
  xlab("")
#CAPITAL

ggplot(dfb, aes(x=capital, color=legalform, legend=FALSE)) + geom_density(size=0.5, alpha=0.4)+ xlim(-100,100)
  ggtitle(str_c('Failed companies by  legal form')) 
#(palette='Dark2')



#################################
########### REGION    ###########


dfb$zone = NaN
dfb$zone = getZones(dfb)
table(dfb$zone)



#SIZE
palette = brewer.pal(length(unique(dfb$zone)),"Dark2")
barplot(prop.table(table(dfb$`zone`,dfb$size), 1) , main='Failed in 2018 by zone',
        xlab="Size",col=palette,  beside=TRUE)
legend(15, 0.95, text.font=0.5,fill=palette,legend=c(unique(dfb$`zone`)),cex = 0.8)

?legend

#boxplots
dfb %>%
  mutate(class = fct_reorder(zone,size, .fun='mean' )) %>%
  ggplot( aes(x=reorder(zone,size), y=size, fill=class)) + 
  geom_violin() +
  xlab("Zone") +
  theme(legend.position="none") +
  xlab("") +
  xlab("")


for (zone in unique(dfb$zone)){
  jpeg(str_c("img/B/Region/",zone,".jpg"))
  df = dfb[dfb$zone == zone,]
  n = length(unique(df$region))
  if (n < 3){n = 3}          
  palette = brewer.pal(n,"Dark2")
  barplot(prop.table(table(df$region,df$size), 1) , main=str_c('Failed company in "',zone,'" in 2018'),
          xlab="Size", col = palette,
          legend = rownames(table(df$region,df$size)), beside=TRUE)
  
  dev.off()
}


#AGE

palette = brewer.pal(length(unique(dfb$zone)),"Dark2")

ggplot(dfb, aes(x=age, color=zone, legend=FALSE)) + geom_density(size=0.5, alpha=0.4)+
  ggtitle(str_c('Failed companies by  zone of Italy')) 


#boxplots
dfb %>%
  mutate(class = fct_reorder(zone,age, .fun='mean' )) %>%
  ggplot( aes(x=reorder(zone,age), y=age, fill=class)) + 
  geom_boxplot() +
  xlab("Zone") +
  theme(legend.position="none") +
  xlab("") +
  xlab("")

for (zone in unique(dfb$zone)){
  
  df = dfb[dfb$zone == zone,]
  
  ggplot(df, aes(x=age, fill=region)) + geom_density(alpha=0.2) +
    ggtitle(str_c('Failed companies in "',zone,'" in 2018'))
  ggsave(str_c("img/B/Region/age/",zone,".jpg"),dpi=300)
  
  df %>%
    mutate(class = fct_reorder(region,age, .fun='mean' )) %>%
    ggplot( aes(x=reorder(region,age), y=age, fill=class)) + 
    geom_boxplot() +
    xlab("Zone") +
    theme(legend.position="none") +
    xlab("") +
    xlab("") +
    ggtitle(str_c('Failed companies in "',zone,'" in 2018'))
  ggsave(str_c("img/B/Region/age/boxplots_",zone,".jpg"),dpi=300)
  

}




#CAPITAL

palette = brewer.pal(length(unique(dfb$zone)),"Dark2")




for (zone in unique(dfb$zone)){
  
  df = dfb[dfb$zone == zone,]
  
  ggplot(df, aes(x=capital, fill=region)) + geom_density(alpha=0.2) + xlim(-100,100)
    ggtitle(str_c('Failed companies in "',zone,'" in 2018'))
  ggsave(str_c("img/B/Region/capital/",zone,".jpg"),dpi=300)
  
}


#################################
############ ATECO  #############



dfb$ateconames = NaN
dfb$ateconames = getAteco(dfb)
table(dfb$ateconames)


par(mfrow=c(4,5))

graphics.off()# par("mar") par(mar=c(1,1,1,1))
colors = c("#023fa5", "#7d87b9", "#bec1d4", "#d6bcc0", "#bb7784", "#8e063b", "#4a6fe3", "#8595e1", "#b5bbe3", "#e6afb9", "#e07b91", "#d33f6a", "#11c638", "#8dd593", "#c6dec7", "#ead3c6", "#f0b98d", "#ef9708", "#0fcfc0", "#9cded6", "#d5eae7", "#f3e1eb", "#f6c4e1", "#f79cd4")




#boxplots AGE
dfb %>%
  mutate(class = fct_reorder(ATECO,age, .fun='mean' )) %>%
  ggplot( aes(x=reorder(ATECO,age), y=age, fill=class)) + 
  geom_boxplot() +
  xlab("ATECO") +
  theme(legend.position="none") +
  xlab("") +
  xlab("") +
  coord_flip()

#violinplots SIZE
dfb %>%
  mutate(class = fct_reorder(ATECO,size, .fun='mean' )) %>%
  ggplot( aes(x=reorder(ATECO,size), y=size, fill=class)) + 
  geom_violin() +
  xlab("ATECO") +
  theme(legend.position="none") +
  xlab("") +
  xlab("") +
  coord_flip()




for (ateco in unique(dfb$ATECO)){
  jpeg(str_c("img/B/ATECO/size/",str_sub(ateco, 1, 3),".jpg"))
  df = dfb[dfb$ATECO == ateco,]
  n = length(unique(df$ATECO10))
  if (n < 3){n = 3}          
 
  if (n<8){
    barplot(prop.table(table(df$ATECO10,df$size), 1) , main=str_c('Failed company of  "',ateco,'" in 2018'),
            xlab="Size", col = colors,legend = rownames(table(df$ATECO10,df$size)) ,beside=TRUE)
    
  }
  else{
    barplot(prop.table(table(df$ATECO10,df$size), 1) , main=str_c('Failed company of  "',ateco,'" in 2018'),
            xlab="Size", col = colors ,beside=TRUE)
    
  legend(65, 1, text.font=0.5,ncol=3,fill=colors,legend = rownames(table(df$ATECO10,df$size)),cex = 0.8)
  }
  
  dev.off()
}


for (ateco in unique(dfb$ATECO)){
  df = dfb[dfb$ATECO == ateco,]
  n = length(unique(df$ATECO10))
  ggplot(df, aes(x=age, fill=ATECO10)) + geom_density(alpha=0.2) +
    ggtitle(str_c('Failed companies of "',ateco,'" in 2018'))
  ggsave(str_c("img/B/ATECO/age/",str_sub(ateco, 1, 3),".jpg"),dpi=300)
  

}


for (ateco in unique(dfb$ATECO)){
  df = dfb[dfb$ATECO == ateco,]
  n = length(unique(df$ATECO10))
  ggplot(df, aes(x=capital, fill=ATECO10)) + geom_density(alpha=0.2) + xlim(-100,100)
    ggtitle(str_c('Failed companies of "',ateco,'" in 2018'))
  ggsave(str_c("img/B/ATECO/capital/",str_sub(ateco, 1, 3),".jpg"),dpi=300)
  
  
}




 # KS test # 

#### ateco ####
tests =  data.frame(row.names = c('age','size'))
for (ateco in unique(dfb$ATECO)){
  sizetest = ks.test(dfb$size[dfb$ATECO == ateco],dfb$size)
  agetest = ks.test(dfb$age[dfb$ATECO == ateco],dfb$age)
  tests[ateco] = c(agetest$statistic,sizetest$statistic)
  
}
write.csv(tests,'img/B/Bateco.csv')


#### region ####

tests =  data.frame(row.names = c('age','size'))
for (region in unique(dfb$region)){
  sizetest = ks.test(dfb$size[dfb$region == region],dfb$size)
  agetest = ks.test(dfb$age[dfb$region == region],dfb$age)
  tests[region] = c(agetest$statistic,sizetest$statistic)
  
}
write.csv(tests,'img/B/Bregion.csv')

#### legalform ####

tests =  data.frame(row.names = c('age','size'))
for (form in unique(dfb$`Legal form`)){
  sizetest = ks.test(dfb$size[dfb$`Legal form` == form],dfb$size)
  agetest = ks.test(dfb$age[dfb$`Legal form` == form],dfb$age)
  tests[form] = c(agetest$statistic,sizetest$statistic)
  
}
write.csv(tests,'img/B/Blegalform.csv')


