# DATA
## Importing Libraries
library(xts)
library(quantmod)
library(data.table)
library(pryr)
library(quantreg)
library(TTR)
library(caTools)
library(roll)
library(rbenchmark)
library(microbenchmark)
library(urca)
library(chron)
library(tseries)
library(PerformanceAnalytics)
library(lubridate)
library(scales)
library(RColorBrewer)
library(dplyr)
library(lattice)
library(grDevices)

## Functions
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotPositions.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_mySR.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")
source("functions/function_testdf.R")
source("functions/grangerTest.R")
source("functions/plotMaxDD.R")

# Sharpe Ratio
mySR <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} 

# Calmar Ratio
myCalmarRatio <- function(x, # x = series of returns
                          # scale parameter = Nt
                          scale) {
  scale * mean(coredata(x), na.rm = TRUE) / 
    maxdrawdown(cumsum(x))$maxdrawdown
}

# Let's change the LC_TIME option to English
Sys.setlocale("LC_TIME", "English")

# Let's define the system time zone as America/New_York (used in the data)
Sys.setenv(TZ = 'America/New_York')

for (selected_quarter in c("2021_Q1", "2021_Q3", "2021_Q4", 
                           "2022_Q2", "2022_Q4", 
                           "2023_Q1", "2023_Q2")) {
  
  message(selected_quarter)
  
  # loading the data for a selected quarter from a sub-directory "data""
  
  filename_ <- paste0("data/data1_", selected_quarter, ".RData")
  
  load(filename_)
  
  # create index of times for this quarter
  
  data.group1 <- get(paste0("data1_", selected_quarter))
  
  times_ <- substr(index(data.group1), 12, 19)
  
  # the following common assumptions were defined:
  # 1.	do not use in calculations the data from the first 
  # and last 10 minutes of the session (9:31-9:40 and 15:51-16:00)
  
  # lets put missing values for these periods
  data.group1["T09:31/T09:45",] <- NA 
  data.group1["T15:46/T16:00",] <-NA
  
  # lets calculate EMA10 and EMA60 for NQ
  data.group1$NQ_EMA10 <- EMA(na.locf(data.group1$NQ), 10)
  data.group1$NQ_EMA60 <- EMA(na.locf(data.group1$NQ), 60)
  
  # SP
  data.group1$SP_EMA10 <- EMA(na.locf(data.group1$SP), 10)
  data.group1$SP_EMA60 <- EMA(na.locf(data.group1$SP), 60)
  
  # Put missing value whenever the original price is missing
  data.group1$NQ_EMA10[is.na(data.group1$NQ)] <- NA
  data.group1$NQ_EMA60[is.na(data.group1$NQ)] <- NA
  data.group1$SP_EMA10[is.na(data.group1$SP)] <- NA
  data.group1$SP_EMA60[is.na(data.group1$SP)] <- NA
  
  
  # Let's calculate the position for the MOMENTUM strategy
  # if fast MA(t-1) > slow MA(t-1) => pos(t) = 1 [long]
  # if fast MA(t-1) <= slow MA(t-1) => pos(t) = -1 [short]
  #  caution - this strategy is always in the market!
  data.group1$positionNQ.mom <- ifelse(lag.xts(data.group1$NQ_EMA10) >
                                         lag.xts(data.group1$NQ_EMA60),
                                       1, -1)
  
  data.group1$positionSP.mom <- ifelse(lag.xts(data.group1$SP_EMA10) >
                                         lag.xts(data.group1$SP_EMA60),
                                       1, -1)
  
  # Remaining assumptions
  # - exit all positions 20 minutes before the session end, i.e. at 15:40
  # - do not trade within the first 30 minutes of stocks quotations (until 10:00)
  data.group1$positioSP.mom[times(times_) <= times("10:00:00") | 
                              times(times_) > times("15:40:00")] <- 0
  
  data.group1$positionNQ.mom[times(times_) <= times("10:00:00") | 
                               times(times_) > times("15:40:00")] <- 0
  
  
  # Fill every missing position with the previous one
  
  data.group1$positionSP.mom <- na.locf(data.group1$positionSP.mom, na.rm = FALSE)
  data.group1$positionNQ.mom <- na.locf(data.group1$positionNQ.mom, na.rm = FALSE)
  
  # calculating gross P&L
  
  data.group1$pnl_grossNQ.mom <- data.group1$positionNQ.mom * diff.xts(data.group1$NQ) * 20
  data.group1$pnl_grossSP.mom <- data.group1$positionSP.mom * diff.xts(data.group1$SP) * 50
  
  
  # number of transactions
  data.group1$ntransSP.mom <- abs(diff.xts(data.group1$positionSP.mom))
  data.group1$ntransNQ.mom <- abs(diff.xts(data.group1$positionNQ.mom))
  
  # bu hisse ilk deyeri 0 edir, cunki position 2 deyer arasindaki ferqe esasen teyin edilir. Ilk deyerde de ferq olmayacagi ucun silirik.
  data.group1$ntransSP.mom[1] <- 0
  data.group1$ntransNQ.mom[1] <- 0
  
  # Net P&L
  data.group1$pnl_netNQ.mom <- data.group1$pnl_grossNQ.mom  -
    data.group1$ntransNQ.mom * 10 # 10$ per transaction
  
  data.group1$pnl_netSP.mom <- data.group1$pnl_grossSP.mom  -
    data.group1$ntransSP.mom * 10 # 10$ per transaction
  
  # total for strategy
  data.group1$pnl_gross.mom <- data.group1$pnl_grossNQ.mom + data.group1$pnl_grossSP.mom
  data.group1$pnl_net.mom <- data.group1$pnl_netNQ.mom + data.group1$pnl_netSP.mom
  
  
  # aggregate P&Ls and number of transactions to daily
  my.endpoints <- endpoints(data.group1, "days")
  
  data.group1.daily <- period.apply(data.group1[,c(grep("pnl", names(data.group1)),
                                                   grep("ntrans", names(data.group1)))],
                                    INDEX = my.endpoints, 
                                    FUN = function(x) colSums(x, na.rm = TRUE))
  
  # Summarize the strategy for this quarter
  
  # Sharpe Ratio
  grossSR = mySR(x = data.group1.daily$pnl_gross.mom, scale = 252)
  netSR = mySR(x = data.group1.daily$pnl_net.mom, scale = 252)
  
  # Calmar Ratio
  grossCR = myCalmarRatio(x = data.group1.daily$pnl_gross.mom, scale = 252)
  netCR = myCalmarRatio(x = data.group1.daily$pnl_net.mom, scale = 252)
  
  # average number of transactions
  av.daily.ntrades = mean(data.group1.daily$ntransSP.mom + 
                            data.group1.daily$ntransNQ.mom, na.rm = TRUE)
  # P&L
  grossPnL = sum(data.group1.daily$pnl_gross.mom)
  netPnL = sum(data.group1.daily$pnl_net.mom)
  # stat
  stat = netCR * max(0, log(abs(netPnL/1000)))
  
  # collecting all statistics for a particular quarter
  quarter_stats <- data.frame(quarter = selected_quarter,
                              assets.group = 1,
                              grossSR,
                              netSR,
                              grossCR,
                              netCR,
                              av.daily.ntrades,
                              grossPnL,
                              netPnL,
                              stat,
                              stringsAsFactors = FALSE)
  
  # collect summaries for all quarters
  if(!exists("quarter_stats.all.group1")) quarter_stats.all.group1 <- quarter_stats else
    quarter_stats.all.group1 <- rbind(quarter_stats.all.group1, quarter_stats)
  
  # create a plot of Gross and Net P&L and save it to png file
  png(filename = paste0("P&L Group 1 - ", selected_quarter, ".png"),
      width = 1000, height = 600)
  
  print( # when plotting in a loop you have to use print()
    plot(cbind(cumsum(data.group1.daily$pnl_gross.mom),
               cumsum(data.group1.daily$pnl_net.mom)),
         multi.panel = FALSE,
         main = paste0("Gross and Net P&L for Group 1 \nquarter ", selected_quarter), 
         col = c("#377EB8", "#E41A1C"),
         major.ticks = "weeks", 
         grid.ticks.on = "weeks",
         grid.ticks.lty = 3,
         legend.loc = "topleft",
         cex = 1))
  # closing the png device (and file)
  dev.off()
  
  # remove all unneeded objects for group 1
  rm(data.group1, my.endpoints, grossSR, netSR, av.daily.ntrades,
     grossPnL, netPnL, stat, quarter_stats, data.group1.daily)
  
  gc()

} # end of the loop

write.csv(quarter_stats.all.group1, 
          "Group 1 - Quarterly Statistics.csv",
          row.names = FALSE)