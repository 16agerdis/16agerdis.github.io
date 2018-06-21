#########USER_INPUT##########
#Date Input (OPTIONAL) / Format: YYYY-MM-DD / If not different date, date should equal 'Default'
userDate = "Default"
# "stack" or "group"
graphType <- "stack"
splitSeasons <- FALSE
exportContour = FALSE
exportMesh = FALSE

#include
library(RODBC)
library(ggplot2)
library(plotly)

ch3 <- odbcConnect("Allegro", uid="uv_user", pwd="uvuser123")

if(userDate == "Default"){
  userDate <- as.character(Sys.Date())
} else{
  userDate <- userDate
}

cYear <- substr(userDate, 1,4)
cMonth <- substr(userDate, 6,7)
if(cMonth<10){
  cMonthP1 <- paste("0",as.numeric(cMonth)+1,sep="")
} else {
  cMonthP1 <- as.numeric(cMonth)+1
}
if(cMonth=="12"){
cStampMin <- as.numeric(paste(as.numeric(cYear)+1,"01",sep=""))
cStampMax <- as.numeric(paste(as.numeric(substr(Sys.Date(),1,4))+2,"01",sep=""))
} else{
  cStampMin <- as.numeric(paste(cYear,cMonthP1,sep=""))
  cStampMax <- as.numeric(paste(as.numeric(substr(Sys.Date(),1,4))+2,cMonthP1,sep=""))
}
#t1.HS_HEDGE_KEY, t1.CM_CONTRACT_MONTH, t1.DEAL_VOL, t1.RECORD_CREATE_DATE, t1.DY_BEG_DAY,
ch2 <- odbcConnect("Allegro", uid="uv_user", pwd="uvuser123")
MDquery <- paste("SELECT * 
FROM mo_user.universal_view t1 
WHERE t1.tablesource = 'ValuationDetail' AND t1.HS_HEDGE_KEY IN ('GCV S RI','GCV S RI A','GCV S RI D','GCV W RI','GCV W RI A', 'GCV W RI D') AND (CM_CONTRACT_MONTH BETWEEN ",cStampMin, " AND ", cStampMax,  
") AND t1.record_create_date >= date '",userDate,"' AND t1.record_create_date < date '", as.character(as.Date(userDate)+1), "' AND t1.deal_type !=ALL ('Power OTC Swap','Power Swaption','UCAP','Capacity Swap','Power OTC Option','Power Physical','NG Physical')", sep= "")

MDquery <- gsub("[\r\n]", "", MDquery)

MDtable <- sqlQuery(ch3, MDquery)


if(splitSeasons){
sumTable <- data.frame(matrix(ncol=7))
nameCol <- c("CONTRACT_MONTH", "GCV S RI", "GCV S RI D", "GCV S RI A", "GCV W RI", "GCV W RI D", "GCV W RI A")
colnames(sumTable) <- nameCol

i<-1
while(i<= length(unique(MDtable$CM_CONTRACT_MONTH))){
  sumTable[i,1] <- unique(MDtable$CM_CONTRACT_MONTH)[i]
  sumTable[i,2] <- sum(MDtable$DEAL_VOL[MDtable$CM_CONTRACT_MONTH == unique(MDtable$CM_CONTRACT_MONTH)[i] & MDtable$HS_HEDGE_KEY == 'GCV S RI' ])
  sumTable[i,3] <- sum(MDtable$DEAL_VOL[MDtable$CM_CONTRACT_MONTH == unique(MDtable$CM_CONTRACT_MONTH)[i] & MDtable$HS_HEDGE_KEY == 'GCV S RI D'])
  sumTable[i,4] <- sum(MDtable$DEAL_VOL[MDtable$CM_CONTRACT_MONTH == unique(MDtable$CM_CONTRACT_MONTH)[i] & MDtable$HS_HEDGE_KEY == 'GCV S RI A'])
  sumTable[i,5] <- sum(MDtable$DEAL_VOL[MDtable$CM_CONTRACT_MONTH == unique(MDtable$CM_CONTRACT_MONTH)[i] & MDtable$HS_HEDGE_KEY == 'GCV W RI'])
  sumTable[i,6] <- sum(MDtable$DEAL_VOL[MDtable$CM_CONTRACT_MONTH == unique(MDtable$CM_CONTRACT_MONTH)[i] & MDtable$HS_HEDGE_KEY == 'GCV W RI D'])
  sumTable[i,7] <- sum(MDtable$DEAL_VOL[MDtable$CM_CONTRACT_MONTH == unique(MDtable$CM_CONTRACT_MONTH)[i] & MDtable$HS_HEDGE_KEY == 'GCV W RI A'])
  i <- i+1
  }
CM <- sumTable$CONTRACT_MONTH
MD.Month <- as.Date(paste(substr(CM,1,4),"-",substr(CM,5,6),"-01",sep=""))
VolSM <- sumTable$`GCV S RI`
VolSD <- sumTable$`GCV S RI D`
VolSA <- sumTable$`GCV S RI A`
VolWM <- sumTable$`GCV W RI`
VolWD <- sumTable$`GCV W RI D`
VolWA <- sumTable$`GCV W RI A`

sumTable <- sumTable[with(sumTable, order(sumTable$CONTRACT_MONTH)),]

p <- plot_ly(MDtable, x = ~MD.Month, y = ~VolSM, type = 'bar', name = 'GCV S RI') %>%
  add_trace(y = ~VolSD, name = 'GCV S RI D') %>%
  add_trace(y = ~VolSA, name = 'GCV S RI A') %>%
  add_trace(y = ~VolWM, name = 'GCV W RI') %>%
  add_trace(y = ~VolWD, name = 'GCV W RI D') %>%
  add_trace(y = ~VolWA, name = 'GCV W RI A') %>%
  layout(yaxis = list(title = 'Deal Volume'), barmode = graphType)
p
}

if(!splitSeasons){
  sumTable <- data.frame(matrix(ncol=7))
  nameCol <- c("CONTRACT_MONTH", "GCV RI", "GCV RI D", "GCV RI A")
  colnames(sumTable) <- nameCol  
  
  i<-1
  while(i<= length(unique(MDtable$CM_CONTRACT_MONTH))){
    sumTable[i,1] <- unique(MDtable$CM_CONTRACT_MONTH)[i]
    sumTable[i,2] <- sum(MDtable$DEAL_VOL[MDtable$CM_CONTRACT_MONTH == unique(MDtable$CM_CONTRACT_MONTH)[i] & (MDtable$HS_HEDGE_KEY == 'GCV S RI' | MDtable$HS_HEDGE_KEY == 'GCV W RI')])
    sumTable[i,3] <- sum(MDtable$DEAL_VOL[MDtable$CM_CONTRACT_MONTH == unique(MDtable$CM_CONTRACT_MONTH)[i] & (MDtable$HS_HEDGE_KEY == 'GCV S RI D' | MDtable$HS_HEDGE_KEY == 'GCV W RI D')])
    sumTable[i,4] <- sum(MDtable$DEAL_VOL[MDtable$CM_CONTRACT_MONTH == unique(MDtable$CM_CONTRACT_MONTH)[i] & (MDtable$HS_HEDGE_KEY == 'GCV S RI A' |  MDtable$HS_HEDGE_KEY == 'GCV W RI A')])
    i <- i+1
  }
  CM <- sumTable$CONTRACT_MONTH
  MD.Month <- as.Date(paste(substr(CM,1,4),"-",substr(CM,5,6),"-01",sep=""))
  VolM <- sumTable$`GCV RI`
  VolD <- sumTable$`GCV RI D`
  VolA <- sumTable$`GCV RI A`
  
  sumTable <- sumTable[with(sumTable, order(sumTable$CONTRACT_MONTH)),]
  
  p <- plot_ly(MDtable, x = ~MD.Month, y = ~VolM, type = 'bar', name = 'GCV RI', xaxis=list(title='Time')) %>%
    add_trace(y = ~VolD, name = 'GCV RI D') %>%
    add_trace(y = ~VolA, name = 'GCV RI A') %>%
    layout(yaxis = list(title = 'Deal Volume'), barmode = graphType)
  p
  }

