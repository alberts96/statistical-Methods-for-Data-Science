
df = aida
summary(df)
names(df)


#DELETING NULL ROW FROM IMPORTANT COLUMNS

df <- df[!(is.na(df$'ATECO 2007code') | is.na(df$'Legal form')),] 

df <- df[!(is.na(df$'Incorporation year') | is.na(df$"Last accounting closing date")),]   

df <- df[!(is.na(df$'Total assetsth EURLast avail. yr') | is.na(df$'Number of employeesLast avail. yr')),] 

df <- df[!(is.na(df$"Profit (loss)th EURLast avail. yr") | is.na(df$"Net working capitalth EURLast avail. yr")),]

df <- df[!(is.na(df$"Profit (loss)th EURLast avail. yr")),]
#from merger or demerger we cannot know if the company is failed or not
df = df[!(df$`Legal status`=="Dissolved (demerger)"),]
df = df[!(df$`Legal status`=="Dissolved (merger)"),]

# Calculating the AGE
df['age'] = df['Last accounting closing date'] - df['Incorporation year']
table(df$`age`)
df=df[!(df$age < 0), ]


#size calculated as (micro impresa, piccola impresa, media imprese, grande impresa)
df['size'] = 3
df$`size`[df$`Number of employeesLast avail. yr`<250 & (df$`Total assetsth EURLast avail. yr`<43000000 | df$"Profit (loss)th EURLast avail. yr"<43000000)] = 2
df$`size`[df$`Number of employeesLast avail. yr`<50 & (df$`Total assetsth EURLast avail. yr`<10000000 | df$"Profit (loss)th EURLast avail. yr"<10000000)] = 1
df$`size`[df$`Number of employeesLast avail. yr`<10 & (df$`Total assetsth EURLast avail. yr`<2000000 | df$"Profit (loss)th EURLast avail. yr"<2000000)] = 0

table(df$size)

#########################
####### ATECO CODES #####
#########################

#Take the first to digits 
df$`ATECO10` = substr(df$`ATECO 2007code`, start = 1, stop = 2)




df$ATECO[df$ATECO10 == "01" | 
           df$ATECO10 == "02" |
           df$ATECO10 == "03"] <- "(A)Pesca e Agricoltura"

df$ATECO[df$ATECO10 == "05" | 
           df$ATECO10 == "06" |
           df$ATECO10 == "07"|
           df$ATECO10 == "08"|
           df$ATECO10 == "09"] <- "(B)Estrazione Minerali"

df$ATECO[df$ATECO10 >= "10" & 
           df$ATECO10 <= "33"] <- "(C)Manifattura"

df$ATECO[df$ATECO10 == "35"] <- "(D)Fornitura Gas/Ele"

df$ATECO[df$ATECO10 >= "36" & 
           df$ATECO10 <= "39"] <- "(E)Gestione Fogne e Rifiuti"

df$ATECO[df$ATECO10 >= "41" & 
           df$ATECO10 <= "43"] <- "(F)Costruzioni"

df$ATECO[df$ATECO10 >= "45" & 
           df$ATECO10 <= "47"] <- "(G)Commercio"

df$ATECO[df$ATECO10 >= "49" & 
           df$ATECO10 <= "53"] <- "(H)Trasporti e Magazzinaggio"

df$ATECO[df$ATECO10 >= "55" & 
           df$ATECO10 <= "56"] <- "(I)Servizi: Alloggio e Ristorazione"

df$ATECO[df$ATECO10 >= "58" & 
           df$ATECO10 <= "63"] <- "(J)Servizi: ICT"

df$ATECO[df$ATECO10 >= "64" & 
           df$ATECO10 <= "66"] <- "(K)Servizi: Finanza e Assic."

df$ATECO[df$ATECO10 == "68"] <- "(L)Servizi: Immobiliari"

df$ATECO[df$ATECO10 >= "69" & 
           df$ATECO10 <= "75"] <- "(M)Servizi: Consulenza e attività scientifiche"

df$ATECO[df$ATECO10 >= "77" & 
           df$ATECO10 <= "82"] <- "(N)Servizi: Viaggi e Noleggio"

df$ATECO[df$ATECO10 == "84"] <- "(O)Servizi: Amm. Pubblica e Difesa"

df$ATECO[df$ATECO10 == "85"] <- "(P)Servizi: IStruzione"

df$ATECO[df$ATECO10 >= "86" & 
           df$ATECO10 <= "88"] <- "(Q)Servizi: Sanità e Ass. Sociale"

df$ATECO[df$ATECO10 >= "90" & 
           df$ATECO10 <= "93"] <- "(R)Attività sportive, artistiche e intratt."

df$ATECO[df$ATECO10 >= "94" & 
           df$ATECO10 <= "96"] <- "(S)Altre attività"

df$ATECO[df$ATECO10 >= "97" & 
           df$ATECO10 <= "98"] <- "(T)Personale Domestico"

df$ATECO[df$ATECO10 == "99"] <- "(U)Organismi extra territoriali"

df<-df[!(df$ATECO10=="00"),]


table(df$`ATECO10`)
table(df$`ATECO`)

#ACTIVED OR FAILED
df$`active` = 0
df$`active`[df$`Legal status`== "Active"] = 1
df$`active`[df$`Legal status`== "Active (default of payments)"] = 1
df$`active`[df$`Legal status`== "Active (receivership)"] = 1

df$status = "active"
df$status[df$active == 0] = "failed"

#Rename
df$'region' = df$'Registered office address - Region'
df$'year' = df$'Last accounting closing date'
df$'ateco' = df$'ATECO10'
df$'capital' = df$"Net working capitalth EURLast avail. yr"

table(df$'Legal form')

df$`Legal form`[
  df$'Legal form'!= "S.C.A.R.L." &
    df$'Legal form'!= "S.C.A.R.L.P.A." & 
    df$'Legal form'!= "S.R.L. one-person" &
    df$'Legal form'!= "S.R.L. simplified" &
    df$'Legal form'!= "S.P.A." &
    df$'Legal form'!= "S.R.L." &
    df$'Legal form'!= "Social cooperative company" &
    df$'Legal form'!= "S.N.C." &
    df$'Legal form'!= "S.A.S."  &
    df$'Legal form'!= "Consortium"] = 'Other'

names(df)

write.csv(df,'dfABC.csv')

rownames(df) = df$`Tax code number` #set tax code as index

# preparign fro A and B
dfAB = df[, c("Legal form", "active", "age", "region", "ateco","status","ATECO","ATECO10", "size", "year","Incorporation year","capital")]
write.csv(dfAB,"dfAB.csv")


df$ROA =df$`Return on asset (ROA)%Last avail. yr`
ggplot(df, aes(x=ROA)) + geom_density(alpha=0.2) + xlim(-100,100)

## Preparing for point C

names(df)

targets = c('Last avail. yr','Year - 1','Year - 2')


columns = c()
for (col in names(df)) {
  if (grepl( targets[1],col, fixed = TRUE) ==TRUE) {
    columns = append(columns,substr(col,start=1,stop=nchar(col)-nchar(targets[1])))

}}


for (col in columns){
  column = paste(col,targets[2],sep='')
  df[is.na(df[,column]),column] = df[is.na(df[column]),paste(col,targets[3],sep='')]
}

for (col in columns){
  column = paste(col,targets[1],sep='')
  df[is.na(df[,column]),column] = df[is.na(df[column]),paste(col,targets[1],sep='')]
}
     
summary(df)


for (col in columns){
  column = paste(col,targets[1],sep='')
  df = df[is.na(df[,column]),column] = df[is.na(df[column]),paste(col,targets[1],sep='')]
}

lastcolumns = c()

for (col in names(df)) {
  if (grepl( targets[1],col, fixed = TRUE) ==TRUE) {
    lastcolumns = append(lastcolumns,col)
    
  }}


table(is.na(df))

for (col in lastcolumns){
  print(col)
  print(table(is.na(df[col])))
  print(table(is.na(aida[col])))
}




selected = lastcolumns
to_remove = c("Banks/turnover%Last avail. yr","Cost of debit (%)%Last avail. yr","Return on investment (ROI) (%)%Last avail. yr") #when there are too much null values, around 1M
selected= selected[! selected %in% to_remove]
selected = append(selected,c("Legal form", "active", "age", "region", "ateco","status","ATECO", "size", "year","Incorporation year"))
  
dfprove = df 
for (col in selected){
  dfprove = dfprove[!is.na(dfprove[col]),]
  
}



table(dfprove$status)
write.csv(dfprove[,selected],'dfC.csv')
