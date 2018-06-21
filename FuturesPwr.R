#########USER_INPUT##########
#Date Input (OPTIONAL) / Format: YYYY-MM-DD / If not different date, date should equal 'Default'
userDate = "Default"

library(RODBC)
library(plotly)

if(userDate == "Default"){
  if(as.POSIXlt(as.Date(substr(Sys.time(), 1, 10)))$wday==1){
    systime <- as.Date(substr(Sys.time(), 1, 10))-3
  } else if (as.POSIXlt(as.Date(substr(Sys.time(), 1, 10)))$wday==0){
    systime <- as.Date(substr(Sys.time(), 1, 10))-2
  } else {
    systime <- as.Date(substr(Sys.time(), 1, 10))-1
  }
  userDate <- as.character(systime)
} else{
  userDate <- userDate  
}

userDate2 <- as.character(as.Date(userDate)-30)

  if (as.POSIXlt(as.Date(substr(userDate2, 1, 10)))$wday==0){
    systime <- as.Date(substr(userDate2, 1, 10))-2
  } else if(as.POSIXlt(as.Date(substr(userDate2, 1, 10)))$wday==6){
    systime <- as.Date(substr(userDate2, 1, 10))-1
  } else {
    systime <- as.Date(substr(userDate2,1,10))
  }
  userDate2 <- as.character(systime)

userYear <- as.numeric(substr(as.Date(userDate),1,4))
userMonth <- as.numeric(substr(as.Date(userDate),6,7))

ch3 <- odbcConnect("Zema2", uid="ze_READONLY", pwd="ze_read_only")

#t.CONTRACT_YEAR, t.CONTRACT_MONTH, t.SETTLEMENT_PRICE, t.HUB

zema4Query <- paste("select t.CONTRACT_YEAR, t.CONTRACT_MONTH, t.SETTLEMENT_PRICE, t.HUB 
                    from ze_data.ice_cleared_power_settle t 
                    where trade_date = date '", userDate, "' and CONTRACT_YEAR <= ",as.numeric(substr(as.Date(userDate),1,4))+3, " and hub = 'NYISO A' and product = 'Peak Futures (50 MW)'", sep="")



zema4 <- sqlQuery(ch3, zema4Query)

zema5Query <- paste("select t.CONTRACT_YEAR, t.CONTRACT_MONTH, t.SETTLEMENT_PRICE, t.HUB 
                    from ze_data.ice_cleared_power_settle t 
                    where ((CONTRACT_YEAR = ", userYear, " and CONTRACT_MONTH >= ", userMonth, ") or CONTRACT_YEAR > ",userYear,") and trade_date = date '", userDate2, "' and CONTRACT_YEAR <= ",as.numeric(substr(as.Date(userDate),1,4))+3, " and hub = 'NYISO A' and product = 'Peak Futures (50 MW)'", sep="")



zema5 <- sqlQuery(ch3, zema5Query)

zema4["Space"]<-""

i<-1
while (i <= nrow(zema4)){
  if(zema4$CONTRACT_MONTH[i]<10){
    zema4$Space[i] <- "0"
  } 
  i <- i+1
}

zema5["Space"]<-""

i<-1
while (i <= nrow(zema5)){
  if(zema5$CONTRACT_MONTH[i]<10){
    zema5$Space[i] <- "0"
  } 
  i <- i+1
}

zema4["CM"] <- paste(zema4$CONTRACT_YEAR,zema4$Space,zema4$CONTRACT_MONTH,sep="")
zema4["Month"] <- as.Date(paste(substr(zema4$CM,1,4),"-",substr(zema4$CM,5,6),"-01",sep=""))
zema4<-zema4[with(zema4,order(zema4$Month)),]
an.Price <- zema4$SETTLEMENT_PRICE


zema5["CM"] <- paste(zema5$CONTRACT_YEAR,zema5$Space,zema5$CONTRACT_MONTH,sep="")
zema5["Month"] <- as.Date(paste(substr(zema5$CM,1,4),"-",substr(zema5$CM,5,6),"-01",sep=""))
zema5<-zema5[with(zema5,order(zema5$Month)),]
ao.Price <- zema5$SETTLEMENT_PRICE

FPA.Month <- zema4$Month

p <- plot_ly(x=~FPA.Month, y=~an.Price, type = "scatter", mode= "lines", name = 'Futures(Current)') %>%
  add_trace(y=~ao.Price, type="scatter", mode="lines", name= 'Futures(30Days-Prior)')

der <- (an.Price - ao.Price)

d <- plot_ly(x=~FPA.Month, y=~der, type = "scatter", mode= "lines",name='Change in Futures')

sp <- subplot(p,d, shareX = TRUE, nrows = 2) %>% layout(title= 'Zone A: Electric Futures vs Futures (30 Days Prior)', xaxis= list(title='Time (Year)'), yaxis = list(title = 'Price (USD)/Dth'), yaxis2 =list(title = 'Change in Price (USD)/Dth'))

odbcCloseAll()