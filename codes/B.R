library(ggplot2)
library(stringr)
library(tidyverse)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(RColorBrewer)

dfAB  <- read.csv(file = 'dfAB.csv')
year = 2018 #2018 is the most populet year

dfb <- dfAB[dfAB$year==year,]
dim(dfb)

dfb$region <- as.character(dfb$region)
dfb$'region'[dfb$'region' == "Valle d'Aosta/Vallée d'Aoste"] <- "Valle d'Aosta"


names(dfAB)

table(dfAB$`ATECO`)


names(dfb)


ggplot(dfb, aes(x=age, fill=status))+ geom_density(position = position_fill())+
  scale_fill_manual(values=c("#04bc3c", "#fc746c")) +ggsave("img/B/age.jpg")


ggplot(dfb, aes(x=size, fill=status))+ geom_bar(position = position_fill())+
  scale_fill_manual(values=c("#04bc3c", "#fc746c"))+ggsave("img/B/size.jpg")



#################################
########### LEGAL FORM ###########

dfb$`Legal.form`[dfb$`Legal.form` == "S.N.C." | dfb$`Legal.form` == "S.A.S." | dfb$`Legal.form` == "Social cooperative company"] = "Other"

dfb$legalform = NaN
for (lf in unique(dfb$`Legal.form`)){
  dfb$`legalform`[dfb$`Legal.form` == lf] = lf
}
table(dfb$`legalform`)
dim(dfb)

for (form in unique(dfb$legalform)){
  
  ggplot(dfb[dfb$legalform==form,], aes(x=age, fill=status))+ geom_density(position = position_fill())+
    ggtitle(form) + scale_fill_manual(values=c("#04bc3c", "#fc746c"))+
    ggsave(str_c("img/B/LegalForm/age/",form,".jpg"))
  
  ggplot(dfb[dfb$legalform==form,], aes(x=size, fill=status))+ geom_bar(position = position_fill())+
    ggtitle(form)+scale_fill_manual(values=c("#04bc3c", "#fc746c"))+
    ggsave(str_c("img/B/LegalForm/size/",form,".jpg"))
  
}


#################################
########### ATECO ###########
table(dfb$ATECO)

for (atecocode in unique(dfb$ATECO)){
  
  ggplot(dfb[dfb$ATECO==atecocode,], aes(x=age, fill=status))+ geom_density(position = position_fill())+
    ggtitle(atecocode) + scale_fill_manual(values=c("#04bc3c", "#fc746c"))+
    ggsave(str_c("img/B/ATECO/age/",str_sub(atecocode, 1, 3),".jpg"))
  
  ggplot(dfb[dfb$ATECO==atecocode,], aes(x=size, fill=status))+ geom_bar(position = position_fill())+
    ggtitle(atecocode)+scale_fill_manual(values=c("#04bc3c", "#fc746c"))+
    ggsave(str_c("img/B/ATECO/size/",str_sub(atecocode, 1, 3),".jpg"))
  
}


#################################
########### REGION ###########

table(dfb$region)

for (region in unique(dfb$region)){
  
  ggplot(dfb[dfb$region==region,], aes(x=age, fill=status))+ geom_density(position = position_fill())+
    ggtitle(region) + scale_fill_manual(values=c("#04bc3c", "#fc746c"))+
    ggsave(str_c("img/B/Region/age/",region,".jpg"))
  
  ggplot(dfb[dfb$region==region,], aes(x=size, fill=status))+ geom_bar(position = position_fill())+
    ggtitle(region)+scale_fill_manual(values=c("#04bc3c", "#fc746c"))+
    ggsave(str_c("img/B/Region/size/",region,".jpg"))
  
}






###########ZONES###########
dfb$zone = NaN
dfb$zone = getZones(dfb)
table(dfb$zone)

for (zone in unique(dfb$zone)){
  print(str_replace(zone, ' ',''))
  ggplot(dfb[dfb$zone==zone,], aes(x=age, fill=status))+ geom_density(position = position_fill())+
    ggtitle(zone) + scale_fill_manual(values=c("#04bc3c", "#fc746c"))
    ggsave(str_c("img/B/zone/age/",str_replace(zone, ' ',''),".jpg"))
  
  ggplot(dfb[dfb$zone==zone,], aes(x=size, fill=status))+ geom_bar(position = position_fill())+
    ggtitle(zone)+scale_fill_manual(values=c("#04bc3c", "#fc746c"))
    ggsave(str_c("img/B/zone/size/",str_replace(zone, ' ',''),".jpg"))
  
}


