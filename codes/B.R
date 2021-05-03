library(ggplot2)
library(stringr)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

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

#AGE

barplot(prop.table(table(dfb$size)) , main='Failed by year and legal form',
        xlab="Size",  beside=TRUE)


ggplot(dfb, aes(x=age, color=`region`)) + geom_density(size=0.8, alpha=0.4)

ggplot(dfb, aes(x=age, fill=dfb$`Legal form`)) + geom_density(alpha=0.4)


#################################
########### REGION    ###########

getZones <- function(dfb) {
  dfb$zone[dfb$`region`== "Sicilia" ] <- "isole"
  dfb$zone[dfb$`region`== "Sardegna" ] <- "isole"
  dfb$zone[dfb$`region`== "Calabria" ] <- "sud"
  dfb$zone[dfb$`region`== "Basilicata" ] <- "sud"
  dfb$zone[dfb$`region`== "Abruzzo" ] <- "sud"
  dfb$zone[dfb$`region`== "Molise" ] <- "sud"
  dfb$zone[dfb$`region`== "Campania" ] <- "sud"
  dfb$zone[dfb$`region`== "Puglia" ] <- "sud"
  dfb$zone[dfb$`region`== "Toscana" ] <- "centro"
  dfb$zone[dfb$`region`== "Umbria" ] <- "centro"
  dfb$zone[dfb$`region`== "Marche" ] <- "centro"
  dfb$zone[dfb$`region`== "Lazio" ] <- "centro"           
  dfb$zone[dfb$`region`== "Trentino-Alto Adige" ] <- "nord est"
  dfb$zone[dfb$`region`== "Veneto" ] <- "nord est"
  dfb$zone[dfb$`region`== "Friuli-Venezia Giulia" ] <- "nord est"
  dfb$zone[dfb$`region`== "Emilia-Romagna" ] <- "nord est"
  dfb$zone[dfb$`region`== "Valle d'Aosta" ] <- "nord ovest"
  dfb$zone[dfb$`region`== "Lombardia" ] <- "nord ovest"
  dfb$zone[dfb$`region`== "Piemonte" ] <- "nord ovest"
  dfb$zone[dfb$`region`== "Liguria" ] <- "nord ovest"
  
  return(dfb$zone)
}

dfb$zone = getZones(dfb)
table(dfb$zone)
