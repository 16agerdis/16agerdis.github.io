"This program produces a graph of data displaying price per Dth by year for future periods of time. The data comes from the 'Gas Forward Basis Report -05-15-2018' which is saved to the hedging file in 'IT'->'Summer Intern 2018 Projects'->'R Project'->'R Scripts'->'GasForward'

Ariana Gerdis 05/22/2018"

library(plotly) #used for creating nice plots
library(readxl) #used for easily reading in and creating tables for Excel files

#read in the forward gas data file
GasForward <- read_excel("Z:/IT/Summer Intern 2018 Projects/R Project/RScripts/GasForward.xlsx")
#View(GasForward)
Gas_Forward_Basis_Report <- GasForward
#View(Gas_Forward_Basis_Report)

#establish variables from data set
#I realized too far into doing this that there were more pipelines than I expected and that I should have just written a loop to do this... Oh well, I'm committed.
Year <- Gas_Forward_Basis_Report$DATE
Price_NYMEX  <- Gas_Forward_Basis_Report$NYMEX_Henry_Hub
Price_Tetco_ETX  <- Gas_Forward_Basis_Report$Tetco_ETX
Price_Tetco_STX  <- Gas_Forward_Basis_Report$Tetco_STX
Price_Tetco_WLA  <- Gas_Forward_Basis_Report$Tetco_WLA
Price_Tetco_ELA  <- Gas_Forward_Basis_Report$Tetco_ELA
Price_Tetco_M1_30_in  <- Gas_Forward_Basis_Report$Tetco_M1_30_in
Price_Tetco_M2  <- Gas_Forward_Basis_Report$Tetco_M2
Price_Tetco_M3  <- Gas_Forward_Basis_Report$Tetco_M3
Price_Texas_Gas_Z1 <- Gas_Forward_Basis_Report$Texas_Gas_Z1
Price_Texas_Gas_SL<- Gas_Forward_Basis_Report$Texas_Gas_SL
Price_Tenn_Z0 <- Gas_Forward_Basis_Report$Tenn_Z0
Price_Tenn_500 <- Gas_Forward_Basis_Report$Tenn_500
Price_Tenn_800 <- Gas_Forward_Basis_Report$Tenn_800
Price_Tenn_Z1 <- Gas_Forward_Basis_Report$Tenn_Z1
Price_Tenn_Z4_219 <- Gas_Forward_Basis_Report$Tenn_Z4_219
Price_Tenn_Z4_300 <- Gas_Forward_Basis_Report$Tenn_Z4_300
Price_Tenn_Z6 <- Gas_Forward_Basis_Report$Tenn_Z6
Price_Transco_Z1 <- Gas_Forward_Basis_Report$Transco_Z1
Price_Transco_Z2 <- Gas_Forward_Basis_Report$Transco_Z2
Price_Transco_Z3 <- Gas_Forward_Basis_Report$Transco_Z3
Price_Transco_Z4 <- Gas_Forward_Basis_Report$Transco_Z4
Price_Transco_Z6_xNY <- Gas_Forward_Basis_Report$Transco_Z6_xNY
Price_Transco_Z6_NY <- Gas_Forward_Basis_Report$Transco_Z6_NY
Price_Iroq_Z2 <- Gas_Forward_Basis_Report$Iroq_Z2
Price_Iroquois_Z1 <- Gas_Forward_Basis_Report$Iroquois_Z1
Price_TCO <- Gas_Forward_Basis_Report$TCO
Price_Dominion_SP <- Gas_Forward_Basis_Report$Dominion_SP
Price_Empress <- Gas_Forward_Basis_Report$Empress
Price_Chicago<- Gas_Forward_Basis_Report$Chicago
Price_AECO <- Gas_Forward_Basis_Report$AECO
Price_Dawn <- Gas_Forward_Basis_Report$Dawn
Price_Niagara <- Gas_Forward_Basis_Report$Niagara
Price_Iroquois_Recpts <- Gas_Forward_Basis_Report$Iroquois_Recpts
Price_Algonquin <- Gas_Forward_Basis_Report$Algonquin
Price_Dominion_NP <- Gas_Forward_Basis_Report$Dominion_NP
Price_Dracut <- Gas_Forward_Basis_Report$Dracut
Price_Tenn_Z5 <- Gas_Forward_Basis_Report$Tenn_Z5
Price_Leidy_Hub <- Gas_Forward_Basis_Report$Leidy_Hub
Price_Transco_Leidy <- Gas_Forward_Basis_Report$Transco_Leidy
Price_Tenn_Z4_313 <- Gas_Forward_Basis_Report$Tenn_Z4_313
Price_Millennium_East_Pool <- Gas_Forward_Basis_Report$Millennium_East_Pool


#graph figure settings
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "Black")

#x-axis settings
x <- list(
  title = "Year",
  titlefont = f)

#y-axis settings
y <- list(
  title = "Price(USD)/Dth")
# range = range(-.5,3.1,2))

#create the plot
forward_gas <- plot_ly(x = ~Year, y=~Price_NYMEX,type = 'scatter',mode='lines+markers', name= 'NYMEX Henry Hub')  %>%
  add_trace(y = ~Price_Tetco_ETX,type='scatter',mode='lines+markers', name= 'Tetco ETX')  %>%
  add_trace(y = ~Price_Tetco_STX,type='scatter',mode='lines+markers', name= 'Tetco STX')  %>%
  add_trace(y = ~Price_Tetco_WLA,type='scatter',mode='lines+markers', name= 'Tetco WLA')  %>%
  add_trace(y = ~Price_Tetco_ELA,type='scatter',mode='lines+markers', name= 'Tetco ELA')  %>%
  add_trace(y = ~Price_Tetco_M1_30_in ,type='scatter',mode='lines+markers', name= 'Tetco M1 30-in') %>%
  add_trace(y = ~Price_Tetco_M2,type='scatter',mode='lines+markers', name= 'Tetco M2')  %>%
  add_trace(y = ~Price_Tetco_M3,type='scatter',mode='lines+markers', name= 'Tetco M3')  %>%
  add_trace(y = ~Price_Texas_Gas_Z1,type='scatter',mode='lines+markers', name= 'Texas Gas Z1')  %>%
  add_trace(y = ~Price_Texas_Gas_SL,type='scatter',mode='lines+markers', name= 'Texas Gas SL')  %>%
  add_trace(y = ~Price_Tenn_Z0,type='scatter',mode='lines+markers', name= 'Tenn Z0')  %>%
  add_trace(y = ~Price_Tenn_500,type='scatter',mode='lines+markers', name= 'Tenn 500')  %>%
  add_trace(y = ~Price_Tenn_800,type='scatter',mode='lines+markers', name= 'Tenn 800')  %>%
  add_trace(y = ~Price_Tenn_Z1,type='scatter',mode='lines+markers', name= 'Tenn Z1')  %>%
  add_trace(y = ~Price_Tenn_Z4_219,type='scatter',mode='lines+markers', name= 'Tenn Z4 219')  %>%
  add_trace(y = ~Price_Tenn_Z4_300,type='scatter',mode='lines+markers', name= 'Tenn Z4 300')  %>%
  add_trace(y = ~Price_Tenn_Z6,type='scatter',mode='lines+markers', name= 'Tenn Z6')  %>%
  add_trace(y = ~Price_Transco_Z1,type='scatter',mode='lines+markers', name= 'Transco Z1')  %>%
  add_trace(y = ~Price_Transco_Z2,type='scatter',mode='lines+markers', name= 'Transco Z2')  %>%
  add_trace(y = ~Price_Transco_Z3,type='scatter',mode='lines+markers', name= 'Transco Z3')  %>%
  add_trace(y = ~Price_Transco_Z4,type='scatter',mode='lines+markers', name= 'Transco Z4')  %>%
  add_trace(y = ~Price_Transco_Z6_xNY,type='scatter',mode='lines+markers', name= 'Transco Z6 xNY')  %>%
  add_trace(y = ~Price_Transco_Z6_NY,type='scatter',mode='lines+markers', name= 'Transco Z6 NY')  %>%
  add_trace(y = ~Price_Iroq_Z2,type='scatter',mode='lines+markers', name= 'Iroq Z2')  %>%
  add_trace(y = ~Price_Iroquois_Z1,type='scatter',mode='lines+markers', name= 'Iroquois Z1')  %>%
  add_trace(y = ~Price_TCO,type='scatter',mode='lines+markers', name= 'TCO')  %>%
  add_trace(y = ~Price_Dominion_SP,type='scatter',mode='lines+markers', name= 'Dominion SP')  %>%
  add_trace(y = ~Price_Empress,type='scatter',mode='lines+markers', name= 'Empress')  %>%
  add_trace(y = ~Price_Chicago,type='scatter',mode='lines+markers', name= 'Chicago')  %>%
  add_trace(y = ~Price_AECO,type='scatter',mode='lines+markers', name= 'AECO')  %>%
  add_trace(y = ~Price_Dawn,type='scatter',mode='lines+markers', name= 'Dawn')  %>%
  add_trace(y = ~Price_Niagara,type='scatter',mode='lines+markers', name= 'Niagara')  %>%
  add_trace(y = ~Price_Iroquois_Recpts,type='scatter',mode='lines+markers', name= 'Iroquois Recpts')  %>%
  add_trace(y = ~Price_Algonquin,type='scatter',mode='lines+markers', name= 'Algonquin')  %>%
  add_trace(y = ~Price_Dominion_NP,type='scatter',mode='lines+markers', name= 'Dominion NP')  %>%
  add_trace(y = ~Price_Dracut,type='scatter',mode='lines+markers', name= 'Dracut')  %>%
  add_trace(y = ~Price_Tenn_Z5,type='scatter',mode='lines+markers', name= 'Tenn Z5')  %>%
  add_trace(y = ~Price_Leidy_Hub,type='scatter',mode='lines+markers', name= 'Leidy Hub')  %>%
  add_trace(y = ~Price_Transco_Leidy,type='scatter',mode='lines+markers', name= 'Transco Leidy')  %>%
  add_trace(y = ~Price_Tenn_Z4_313,type='scatter',mode='lines+markers', name= 'Tenn Z4 313')  %>%
  add_trace(y = ~Price_Millennium_East_Pool,type='scatter',mode='lines+markers', name= 'Millennium East Pool')  %>%
  
  layout(yaxis = y, title = "Price(USD)/Dth vs. Year") 


print(forward_gas) #you can easily analyze this graph in Dashboard by utilizing the legend and "hover" function

