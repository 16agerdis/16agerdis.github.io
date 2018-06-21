#########USER_INPUT##########
#Date Input (OPTIONAL) / Format: YYYY-MM-DD / If not different date, date should equal 'Default'
userDate = "Default"
DisplayMeshBool = TRUE
DisplayContourBool = TRUE
exportContour = TRUE
exportMesh = TRUE

#include
library(RODBC)
library(plotly)

ch3 <- odbcConnect("Zema2", uid="ze_READONLY", pwd="ze_read_only")

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

zema3Query <- paste("select distinct t.CONTRACT_YEAR, t.CONTRACT_MONTH, t.STRIKE, t.VOLATILITY 
from ze_data.ice_cleared_ecxopt_settle t 
where t.trade_date = date '", userDate,
"' and t.hub='PJM WH RT'and t.product ='Peak Futures (50 MW)' and t.CONTRACT_TYPE = 'C'", sep="")

zema3Query <- gsub("[\r\n]", "", zema3Query)

zema3 <- sqlQuery(ch3, zema3Query)

zema4Query <- paste("select distinct t.CONTRACT_YEAR, t.CONTRACT_MONTH, t.SETTLEMENT_PRICE 
from ze_data.ice_cleared_power_settle t 
where trade_date = date '", userDate, "' and CONTRACT_YEAR <= ",as.numeric(substr(as.Date(userDate),1,4))+3, " and hub='PJM WH RT' and product ='Peak Futures (50 MW)'", sep="")

zema4Query <- gsub("[\r\n]", "", zema4Query)

zema4 <- sqlQuery(ch3, zema4Query)

zema4["timestamp"] <- as.Date(paste(zema4$CONTRACT_YEAR,"-",zema4$CONTRACT_MONTH,"-01",sep=""))
volRef <- zema4[zema4$timestamp>=as.Date("2017-08-01"), c("timestamp","SETTLEMENT_PRICE")]
zema3["timestamp"] <- as.Date(paste(zema3$CONTRACT_YEAR,"-",zema3$CONTRACT_MONTH,"-01",sep=""))

close(ch3)
volPerc <- c()
i<-1
while(i<=nrow(volRef)-1){
volPerc<-rbind(volPerc, zema3[volRef$timestamp[i]==zema3$timestamp & zema3$STRIKE <= volRef$SETTLEMENT_PRICE[i]*1.20 & zema3$STRIKE >= volRef$SETTLEMENT_PRICE[i]*0.80,c("timestamp","VOLATILITY","STRIKE")],
      zema3[volRef$timestamp[i+1]==zema3$timestamp & zema3$STRIKE <= volRef$SETTLEMENT_PRICE[i+1]*1.20 & zema3$STRIKE >= volRef$SETTLEMENT_PRICE[i+1]*0.80,c("timestamp","VOLATILITY","STRIKE")])
  i<-i+1
}

volPerc <- volPerc[with(volPerc, order(timestamp)),]
volRef <- volRef[with(volRef, order(timestamp)),]

if(volPerc[1,1] != volRef[1,1]){
  volRef <- volRef[2:nrow(volRef),]
}


volRef["Volatility"] <- NA

if(FALSE){
v <- nrow(volRef)
j<-1
while(j<=v){
volRef$Volatility[j] <- max(volPerc[volRef$timestamp[j]==volPerc$timestamp,"VOLATILITY"])
j <- j+1
}
}

if(FALSE){
  v <- nrow(volRef)
  j<-1
  while(j<=v){
    a <- volPerc[volRef$timestamp[j]==volPerc$timestamp,"STRIKE"]
    b <- volRef$SETTLEMENT_PRICE[j]
    if(length(volPerc[volPerc$STRIKE == (min(abs(abs(a)-b))+b), "VOLATILITY"])==0){
    volRef$Volatility[j] <- volPerc[volPerc$STRIKE == (b-min(abs(abs(a)-b))), "VOLATILITY"]
    }
    else {
      volRef$Volatility[j] <- volPerc[volPerc$STRIKE == (min(abs(abs(a)-b))+b), "VOLATILITY"]
    }
    j <- j + 1
  }
}

if(TRUE){
  v <- nrow(volRef)
  k<-0
  i<-1
  j<-1
  while(j<=v){
    while(i<nrow(volPerc[volRef$timestamp[i]==volPerc$timestamp,])+k){
      volRef <- rbind(volRef[1:i,],volRef[i,],volRef[(i+1):nrow(volRef),])
      i<-i+1
    }
    k<-i
    i<-i+1
    j <- j+1
  }
  volRef<- volRef[1:(i-1),]
}

Time <- volPerc$timestamp
Strike <- volPerc$STRIKE
Volatility <- volPerc$VOLATILITY
VolatilityMax <- volRef$Volatility

volContour <- plot_ly(x = ~Time, y = ~Strike, z = ~Volatility, type = 'contour') %>% add_trace(y=~volRef$SETTLEMENT_PRICE, type='scatter',mode='lines',z="")
volMesh <- plot_ly(x = ~Time, y = ~Strike, z = ~Volatility, type = 'mesh3d') 

#volMesh
#volContour

if(exportContour){
  htmlwidgets::saveWidget(as_widget(volContour), paste("\\\\nyhcbdrs06v\\shared\\Regulated electric load\\NYCommodity\\Pricing\\Summer Intern 2017 Projects\\Volatility\\",userDate,"_Contour.html",sep=""))
}

if(exportMesh){
  htmlwidgets::saveWidget(as_widget(volMesh), paste("\\\\nyhcbdrs06v\\shared\\Regulated electric load\\NYCommodity\\Pricing\\Summer Intern 2017 Projects\\Volatility\\",userDate,"_Mesh.html",sep=""))
}

if(DisplayMeshBool){
  volMesh
}

if(DisplayContourBool){
  volContour
}