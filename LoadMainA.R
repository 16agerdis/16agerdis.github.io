###########USER INPUT###########

#Zone: A or F
zone<-'A'

#Time Entry: YYYYMM
userTimeEntry <- 201707

#Most Recent Date in pwr.NGLoad Database
maxCommodityLoadDate <- 201707

#Load or Price Plot
loadPlotBool <- TRUE
#Max year in hours excel file
maxHoursDate <- 2027


###########SETUP###########
#include libraries
library(RODBC)
library(readxl)
library(timeDate)
library(xlsx)
library(plotly)
library(writexl)


#Set Future Boolean
if(userTimeEntry>maxCommodityLoadDate){
  futureBool <- TRUE
} else {
  futureBool <- FALSE
}

#initializers
monthlyFlatLoad <- 0
monthlyPeakLoad <- 0
monthlyPeakWeekendLoad <- 0
monthlyOffPeakLoad <- 0
monthlyFlatPrice <- 0
monthlyPeakPrice <- 0
monthlyPeakWeekendPrice <- 0
monthlyOffPeakPrice <- 0


#access melpiweb(load data NG), Allegro(hedging data), and Zema(LBMP pricing data) database
ch1 <- odbcConnect("melpiweb")
ch2 <- odbcConnect("Allegro", uid="uv_user", pwd="uvuser123")
ch3 <- odbcConnect("Zema", uid="ze_READONLY", pwd="ze_read_only")

ch5 <- odbcConnect("GWHForecast")
ch6 <- odbcConnect("LossFactors")
ch7 <- odbcConnect("Zema2", uid="ze_READONLY", pwd="ze_read_only")
###########HOURS_DATA###########
#WeekendCalc
x<-1
weekendHours <- data.frame(matrix(0, nrow=204, ncol=1))
colnames(weekendHours)[1] <- "Off-Peak Days"
Dates <- substr(holidayNERC(2011:maxHoursDate)@Data,1,10)
Holidays <- data.frame(Dates)
date <- as.Date("2011-01-01")
while(date <= as.Date(paste(as.character(maxHoursDate),"-12-31",sep=""))){
  if(as.character(date) %in% Holidays$Dates | as.POSIXlt(date)$wday==0 | as.POSIXlt(date)$wday==6){
    weekendHours[x,1] <- weekendHours[x,1] + 1
  }
  if(as.numeric(format(date,"%m")) != as.numeric(format(date+1,"%m"))){
    x <- x+1
  }
  date <- date + 1
}
weekendHours["Weekend"] <- weekendHours$`Off-Peak Days`*16
###########LOSS_FACTOR###########
userYear <- substr(userTimeEntry, 1, 4)
userMonth <-substr(userTimeEntry, 5, 6)
userYearNum <- as.numeric(userYear)
userMonthNum <- as.numeric(userMonth)

###########LOAD_DATA###########
#create database specific variables
stringTimeEntry <- as.character(userTimeEntry)
if(as.Date(paste(userYear,"-",userMonth,"-01",sep=""))>Sys.Date()){
  createDate <- Sys.Date()
  createDatePlusOne <- Sys.Date()+1
} else {
  createDate <- paste(userYear,"-",userMonth,"-","01",sep="")
  createDatePlusOne <- paste(userYear,"-",userMonth,"-","02",sep="")
}

#Gather load data from melpiweb
if(zone == 'A' & !futureBool){
  gridQueryA <- paste("SELECT timestamp, commodityLoad FROM pwr.NGLoad WHERE DATEPART(year, timestamp) = ", userYear," AND DATEPART(month, timestamp) = ", userMonth, " AND ngsubzoneid = ", "6", " ORDER BY timestamp", sep="")
  gridQueryB <- paste("SELECT timestamp, commodityLoad FROM pwr.NGLoad WHERE DATEPART(year, timestamp) = ", userYear," AND DATEPART(month, timestamp) = ", userMonth, " AND ngsubzoneid = ", "3", " ORDER BY timestamp", sep="")
  gridQueryC <- paste("SELECT timestamp, commodityLoad FROM pwr.NGLoad WHERE DATEPART(year, timestamp) = ", userYear," AND DATEPART(month, timestamp) = ", userMonth, " AND ngsubzoneid = ", "2", " ORDER BY timestamp", sep="")
  gridQueryD <- paste("SELECT timestamp, commodityLoad FROM pwr.NGLoad WHERE DATEPART(year, timestamp) = ", userYear," AND DATEPART(month, timestamp) = ", userMonth, " AND ngsubzoneid = ", "5", " ORDER BY timestamp", sep="")
  gridQueryE <- paste("SELECT timestamp, commodityLoad FROM pwr.NGLoad WHERE DATEPART(year, timestamp) = ", userYear," AND DATEPART(month, timestamp) = ", userMonth, " AND ngsubzoneid = ", "4", " ORDER BY timestamp", sep="")
  
  gridQueryA <- gsub("[\r\n]", "", gridQueryA)
  gridQueryB <- gsub("[\r\n]", "", gridQueryB)
  gridQueryC <- gsub("[\r\n]", "", gridQueryC)
  gridQueryD <- gsub("[\r\n]", "", gridQueryD)
  gridQueryE <- gsub("[\r\n]", "", gridQueryE)
  
  gridDataA <- sqlQuery(ch1, gridQueryA)
  gridDataB <- sqlQuery(ch1, gridQueryB)
  gridDataC <- sqlQuery(ch1, gridQueryC)
  gridDataD <- sqlQuery(ch1, gridQueryD)
  gridDataE <- sqlQuery(ch1, gridQueryE)
  
  names(gridDataA)[2] <- "commodityLoadA"
  names(gridDataB)[2] <- "commodityLoadB"
  names(gridDataC)[2] <- "commodityLoadC"
  names(gridDataD)[2] <- "commodityLoadD"
  names(gridDataE)[2] <- "commodityLoadE"
  
  #Calculate zonal percentages
  gridData <- cbind(gridDataA,cbind(gridDataB$commodityLoadB,cbind(gridDataC$commodityLoadC,cbind(gridDataD$commodityLoadD,gridDataE$commodityLoadE))))
  gridData["Load"] <- rowSums(gridData[,2:6])
  gridData["PercA"] <- gridData[,2]/gridData$Load
  gridData["PercB"] <- gridData[,3]/gridData$Load
  gridData["PercC"] <- gridData[,4]/gridData$Load
  gridData["PercD"] <- gridData[,5]/gridData$Load
  gridData["PercE"] <- gridData[,6]/gridData$Load
  
  #get rid of excess columns in gridData
  gridData <- data.frame("timestamp" = gridData$timestamp, "Load" = gridData$Load, "PercA" = gridData$PercA, "PercB" = gridData$PercB, "PercC" = gridData$PercC, "PercD" = gridData$PercD, "PercE" = gridData$PercE)
}


#load data from melpiweb if zone 'F' is selected
if(zone == 'F' & !futureBool){
  gridQueryF <- paste("SELECT timestamp, commodityLoad FROM pwr.NGLoad WHERE DATEPART(year, timestamp) = ", userYear," AND DATEPART(month, timestamp) = ", userMonth, " AND ngsubzoneid = ", "1", " ORDER BY timestamp", sep="")
  
  gridQueryF <- gsub("[\r\n]", "", gridQueryF)
  
  gridDataF <- sqlQuery(ch1, gridQueryF)
  
  gridData <- gridDataF
  
  colnames(gridData)[2] <- "Load"
}


###########SWAP_DATA###########
#Gather swap records from allegro database
#Some Options: PRICE, DEAL_VOL, UT_UNIT, CM_CONTRACT_MONTH, DEAL_TYPE, BP_BASIS_POINT, BLOCK, M2M_PRICE, CONTRACT_TYPE, tablesource
oracleQuery <- paste("
                     SELECT
                     t.PRICE, t.DEAL_VOL, t.BLOCK, t.record_create_date, t.trade_date
                     FROM 
                     mo_user.universal_view t
                     WHERE
                     BP_BASIS_POINT = 'NYISO ZONE ", zone, "' AND
                     tablesource = 'ValuationDetail' AND
                     deal_type = 'Power OTC Swap' AND
                     CM_CONTRACT_MONTH = ", userTimeEntry,
                     " AND record_create_date >= date '",createDate , 
                     "' AND record_create_date < date '",createDatePlusOne , "'", sep="")
oracleQuery <- gsub("[\r\n]", "", oracleQuery)
allegroData <- sqlQuery(ch2, oracleQuery)

#Import Hourly Data
hoursData <- read_excel("\\\\nyhcbdrs06v\\shared\\Regulated electric load\\NYCommodity\\Pricing\\Summer Intern 2017 Projects\\R-Project\\References\\Days and Hours Copy.xlsx", sheet = 4)
hoursData["Weekend"] <- weekendHours$Weekend
#Move 'hoursData' to 'allegroData'
allegroData["Flat"] <- hoursData[hoursData$Term==userTimeEntry,][1,2]
allegroData["OnPeak"] <- hoursData[hoursData$Term==userTimeEntry,][1,3]
allegroData["OffPeak"] <- hoursData[hoursData$Term==userTimeEntry,][1,4]
allegroData["Weekend"] <- hoursData[hoursData$Term==userTimeEntry,][1,6]
allegroData["HourlyLoad"] <- NA

#Calculate load and price for every hour
i <- 1
while(i<=nrow(allegroData)){
  if (allegroData$BLOCK[i] == '7X24'){
    allegroData$HourlyLoad[i] <- allegroData$DEAL_VOL[i]/allegroData$Flat[i]
    monthlyFlatLoad <- monthlyFlatLoad + allegroData$HourlyLoad[i]
    monthlyFlatPrice <- monthlyFlatPrice + allegroData$PRICE[i]*allegroData$HourlyLoad[i]
  }
  if (allegroData$BLOCK[i] == '5X16'){
    allegroData$HourlyLoad[i] <- allegroData$DEAL_VOL[i]/allegroData$OnPeak[i]
    monthlyPeakLoad <- monthlyPeakLoad + allegroData$HourlyLoad[i]
    monthlyPeakPrice <- monthlyPeakPrice + allegroData$PRICE[i]*allegroData$HourlyLoad[i]
  }
  if (allegroData$BLOCK[i] == '2X16'){
    allegroData$HourlyLoad[i] <- allegroData$DEAL_VOL[i]/allegroData$Weekend[i]
    monthlyPeakWeekendLoad <- monthlyPeakWeekendLoad + allegroData$HourlyLoad[i]
    monthlyPeakWeekendPrice <- monthlyPeakWeekendPrice +allegroData$PRICE[i]*allegroData$HourlyLoad[i]
  }
  if (allegroData$BLOCK[i] == '5x8 2x24'){
    allegroData$HourlyLoad[i] <- allegroData$DEAL_VOL[i]/allegroData$OffPeak[i]
    monthlyOffPeakLoad <- monthlyOffPeakLoad + allegroData$HourlyLoad[i]
    monthlyOffPeakPrice <- monthlyOffPeakPrice + allegroData$PRICE[i]*allegroData$HourlyLoad[i]
  }  
  i = i + 1  
}

if(monthlyPeakWeekendLoad > 0){
  weekendBool = TRUE
} else {
  weekendBool = FALSE
}
if(monthlyOffPeakLoad > 0){
  offPeakBool = TRUE
} else {
  offPeakBool = FALSE
}

if(!futureBool){
  
  gridData["HedgePower"] <- NA
  gridData["HedgePrice"] <- NA
  
  j<-1
  while(j<=nrow(gridData)){
    date <- paste(substr(gridData$timestamp[j], 1, 10))
    if(as.POSIXlt(gridData$timestamp)$wday[j] != 0 & as.POSIXlt(gridData$timestamp)$wday[j] != 6 & !(date %in% Holidays$Dates)){
      if(as.numeric(format(gridData$timestamp[j],"%H")) >= 7 & as.numeric(format(gridData$timestamp[j],"%H")) < 23){
        gridData$HedgePower[j] <- monthlyFlatLoad + monthlyPeakLoad
        gridData$HedgePrice[j] <- monthlyFlatPrice + monthlyPeakPrice
      } else{
        gridData$HedgePower[j] <- monthlyFlatLoad
        gridData$HedgePrice[j] <- monthlyFlatPrice
      }
    } else{
      gridData$HedgePower[j] <- monthlyFlatLoad
      gridData$HedgePrice[j] <- monthlyFlatPrice
    }
    if((as.POSIXlt(gridData$timestamp)$wday[j] == 0 | as.POSIXlt(gridData$timestamp)$wday[j] == 6 | date %in% Holidays$Dates) & weekendBool){
      if(as.numeric(format(gridData$timestamp[j],"%H")) >= 7 & as.numeric(format(gridData$timestamp[j],"%H")) < 23){
        gridData$HedgePower[j] <- gridData$HedgePower[j] + monthlyPeakWeekendLoad
        gridData$HedgePrice[j] <- gridData$HedgePrice[j] + monthlyPeakWeekendPrice
      }
    }
    if((as.POSIXlt(gridData$timestamp)$wday[j] == 0 | as.POSIXlt(gridData$timestamp)$wday[j] == 6 | date %in% Holidays$Dates) & offPeakBool){
      gridData$HedgePower[j] <- gridData$HedgePower[j] + monthlyOffPeakLoad
      gridData$HedgePrice[j] <- gridData$HedgePrice[j] + monthlyOffPeakPrice
    }
    if(as.POSIXlt(gridData$timestamp)$wday[j] != 0 & as.POSIXlt(gridData$timestamp)$wday[j] != 6 & !(date %in% Holidays$Dates) & offPeakBool){
      if(as.numeric(format(gridData$timestamp[j],"%H")) < 7 | as.numeric(format(gridData$timestamp[j],"%H")) >= 23){
        gridData$HedgePower[j] <- gridData$HedgePower[j] + monthlyOffPeakLoad
        gridData$HedgePrice[j] <- gridData$HedgePrice[j] + monthlyOffPeakPrice
      }
    }
    j = j + 1
  }}
###########LBMP_DATA###########

#gather LBMP data from Zema database
if(zone == 'A' & !futureBool){
  zemaQueryA <- paste("select PRICE from ze_data.NY_ISO_DAMLBMP where to_char(opr_date,'YYYYMM') = '", stringTimeEntry, "' AND INDICE = 'WEST' ORDER BY OPR_DATE, OPR_HOUR", sep ="")
  zemaQueryB <- paste("select PRICE from ze_data.NY_ISO_DAMLBMP where to_char(opr_date,'YYYYMM') = '", stringTimeEntry, "' AND INDICE = 'GENESE' ORDER BY OPR_DATE, OPR_HOUR", sep ="")
  zemaQueryC <- paste("select PRICE from ze_data.NY_ISO_DAMLBMP where to_char(opr_date,'YYYYMM') = '", stringTimeEntry, "' AND INDICE = 'CENTRL' ORDER BY OPR_DATE, OPR_HOUR", sep ="")
  zemaQueryD <- paste("select PRICE from ze_data.NY_ISO_DAMLBMP where to_char(opr_date,'YYYYMM') = '", stringTimeEntry, "' AND INDICE = 'NORTH' ORDER BY OPR_DATE, OPR_HOUR", sep ="")
  zemaQueryE <- paste("select PRICE from ze_data.NY_ISO_DAMLBMP where to_char(opr_date,'YYYYMM') = '", stringTimeEntry, "' AND INDICE = 'MHK VL' ORDER BY OPR_DATE, OPR_HOUR", sep ="")
  
  zemaQueryA <- gsub("[\r\n]", "", zemaQueryA)
  zemaQueryB <- gsub("[\r\n]", "", zemaQueryB)
  zemaQueryC <- gsub("[\r\n]", "", zemaQueryC)
  zemaQueryD <- gsub("[\r\n]", "", zemaQueryD)
  zemaQueryE <- gsub("[\r\n]", "", zemaQueryE)
  
  zemaDataA <- sqlQuery(ch3, zemaQueryA)
  zemaDataB <- sqlQuery(ch3, zemaQueryB)
  zemaDataC <- sqlQuery(ch3, zemaQueryC)
  zemaDataD <- sqlQuery(ch3, zemaQueryD)
  zemaDataE <- sqlQuery(ch3, zemaQueryE)
  
  #attach relevant data to 'gridData' table
  gridData["HedgeMarginalPrice"]<-gridData$HedgePrice/gridData$HedgePower
  gridData["WeightedLBMP"] <- (zemaDataA$PRICE*gridData$PercA)+(zemaDataB$PRICE*gridData$PercB)+(zemaDataC$PRICE*gridData$PercC)+(zemaDataD$PRICE*gridData$PercD)+(zemaDataE$PRICE*gridData$PercE)
}

#gather LBMP data from Zema database if zone 'F' is selected
if((zone == 'F' | zone == 'f') & !futureBool){
  zemaQueryF <- paste("select PRICE from ze_data.NY_ISO_DAMLBMP where to_char(opr_date,'YYYYMM') = '", stringTimeEntry, "' AND INDICE = 'CAPITL' ORDER BY OPR_DATE, OPR_HOUR", sep ="")
  zemaQueryF <- gsub("[\r\n]", "", zemaQueryF)
  
  zemaDataF <- sqlQuery(ch3, zemaQueryF)
  
  #attach relevant data to 'gridData' table
  gridData["HedgeMarginalPrice"]<-gridData$HedgePrice/gridData$HedgePower
  gridData["WeightedLBMP"] <- zemaDataF$PRICE
}


if(futureBool){
  HourlyLoadPrediction_MM["HedgePower"] <- NA
  HourlyLoadPrediction_MM["HedgePrice"] <- NA
  
  j<-1
  while(j<=nrow(HourlyLoadPrediction_MM)){
    date <- paste(substr(HourlyLoadPrediction_MM$timestamp[j], 1, 10))
    if(as.POSIXlt(HourlyLoadPrediction_MM$timestamp)$wday[j] != 0 & as.POSIXlt(HourlyLoadPrediction_MM$timestamp)$wday[j] != 6 & !(date %in% Holidays$Dates)){
      if(as.numeric(format(HourlyLoadPrediction_MM$timestamp[j],"%H")) >= 7 & as.numeric(format(HourlyLoadPrediction_MM$timestamp[j],"%H")) < 23){
        HourlyLoadPrediction_MM$HedgePower[j] <- monthlyFlatLoad + monthlyPeakLoad
        HourlyLoadPrediction_MM$HedgePrice[j] <- monthlyFlatPrice + monthlyPeakPrice
      } else{
        HourlyLoadPrediction_MM$HedgePower[j] <- monthlyFlatLoad
        HourlyLoadPrediction_MM$HedgePrice[j] <- monthlyFlatPrice
      }
    } else{
      HourlyLoadPrediction_MM$HedgePower[j] <- monthlyFlatLoad
      HourlyLoadPrediction_MM$HedgePrice[j] <- monthlyFlatPrice
    }
    if((as.POSIXlt(HourlyLoadPrediction_MM$timestamp)$wday[j] == 0 | as.POSIXlt(HourlyLoadPrediction_MM$timestamp)$wday[j] == 6 | date %in% Holidays$Dates) & weekendBool){
      if(as.numeric(format(HourlyLoadPrediction_MM$timestamp[j],"%H")) >= 7 & as.numeric(format(HourlyLoadPrediction_MM$timestamp[j],"%H")) < 23){
        HourlyLoadPrediction_MM$HedgePower[j] <- HourlyLoadPrediction_MM$HedgePower[j] + monthlyPeakWeekendLoad
        HourlyLoadPrediction_MM$HedgePrice[j] <- HourlyLoadPrediction_MM$HedgePrice[j] + monthlyPeakWeekendPrice
      }
    }
    if((as.POSIXlt(HourlyLoadPrediction_MM$timestamp)$wday[j] == 0 | as.POSIXlt(HourlyLoadPrediction_MM$timestamp)$wday[j] == 6 | date %in% Holidays$Dates) & offPeakBool){
      HourlyLoadPrediction_MM$HedgePower[j] <- HourlyLoadPrediction_MM$HedgePower[j] + monthlyOffPeakLoad
      HourlyLoadPrediction_MM$HedgePrice[j] <- HourlyLoadPrediction_MM$HedgePrice[j] + monthlyOffPeakPrice
    }
    if(as.POSIXlt(HourlyLoadPrediction_MM$timestamp)$wday[j] != 0 & as.POSIXlt(HourlyLoadPrediction_MM$timestamp)$wday[j] != 6 & !(date %in% Holidays$Dates) & offPeakBool){
      if(as.numeric(format(HourlyLoadPrediction_MM$timestamp[j],"%H")) < 7 | as.numeric(format(HourlyLoadPrediction_MM$timestamp[j],"%H")) >= 23){
        HourlyLoadPrediction_MM$HedgePower[j] <- HourlyLoadPrediction_MM$HedgePower[j] + monthlyOffPeakLoad
        HourlyLoadPrediction_MM$HedgePrice[j] <- HourlyLoadPrediction_MM$HedgePrice[j] + monthlyOffPeakPrice
      }
    }
    j = j + 1
  }
}


###########CLOSE_CONNECTIONS########## 
odbcCloseAll()

###########VISUAL###########
if (loadPlotBool & !futureBool){
  HedgeCommodityPlot <- plot_ly(x=~gridData$timestamp,y=~gridData$Load,type='scatter',mode='lines',name='Commodity Load') %>% add_lines(y=~gridData$HedgePower,type='scatter',mode='lines',name='Hedge Power') %>% layout(xaxis=list(title='Time'),yaxis=list(title='Load'))
  htmlwidgets::saveWidget(as_widget(HedgeCommodityPlot), paste("Z:\\IT\\Summer Intern 2018 Projects\\R Project\\Plots\\Futures\\", stringTimeEntry, "_", zone, "_Load.html", sep=""))
}