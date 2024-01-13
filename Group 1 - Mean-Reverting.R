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
  data.group1$NQ_EMA50 <- EMA(na.locf(data.group1$NQ), 50)
  
  # SP
  data.group1$SP_EMA50 <- EMA(na.locf(data.group1$SP), 50)
  
  # Put missing value whenever the original price is missing
  data.group1$NQ_EMA50[is.na(data.group1$NQ)] <- NA
  data.group1$SP_EMA50[is.na(data.group1$SP)] <- NA
  
  
  # Let's calculate the position for the Mean-Reverting strategy
  data.group1$positionNQ.mr <- ifelse(lag.xts(data.group1$NQ_EMA50) >
                                         lag.xts(data.group1$NQ),
                                       1, -1)
  
  data.group1$positionSP.mr <- ifelse(lag.xts(data.group1$SP_EMA50) >
                                         lag.xts(data.group1$SP),
                                       1, -1)
  
  # Remaining assumptions
  # - exit all positions 20 minutes before the session end, i.e. at 15:40
  # - do not trade within the first 30 minutes of stocks quotations (until 10:00)
  data.group1$positioSP.mr[times(times_) <= times("10:00:00") | 
                              times(times_) > times("15:40:00")] <- 0
  
  data.group1$positionNQ.mr[times(times_) <= times("10:00:00") | 
                               times(times_) > times("15:40:00")] <- 0
  
  
  # Fill every missing position with the previous one
  
  data.group1$positionSP.mr <- na.locf(data.group1$positionSP.mr, na.rm = FALSE)
  data.group1$positionNQ.mr <- na.locf(data.group1$positionNQ.mr, na.rm = FALSE)
  
  # calculating gross P&L
  
  data.group1$pnl_grossNQ.mr <- data.group1$positionNQ.mr * diff.xts(data.group1$NQ) * 20
  data.group1$pnl_grossSP.mr <- data.group1$positionSP.mr * diff.xts(data.group1$SP) * 50
  
  
  # number of transactions
  data.group1$ntransSP.mr <- abs(diff.xts(data.group1$positionSP.mr))
  data.group1$ntransNQ.mr <- abs(diff.xts(data.group1$positionNQ.mr))
  
  # bu hisse ilk deyeri 0 edir, cunki position 2 deyer arasindaki ferqe esasen teyin edilir. Ilk deyerde de ferq olmayacagi ucun silirik.
  data.group1$ntransSP.mr[1] <- 0
  data.group1$ntransNQ.mr[1] <- 0
  
  # Net P&L
  data.group1$pnl_netNQ.mr <- data.group1$pnl_grossNQ.mr  -
    data.group1$ntransNQ.mr * 10 # 10$ per transaction
  
  data.group1$pnl_netSP.mr <- data.group1$pnl_grossSP.mr  -
    data.group1$ntransSP.mr * 10 # 10$ per transaction
  
  # total for strategy
  data.group1$pnl_gross.mr <- data.group1$pnl_grossNQ.mr + data.group1$pnl_grossSP.mr
  data.group1$pnl_net.mr <- data.group1$pnl_netNQ.mr + data.group1$pnl_netSP.mr
  
  # aggregate P&Ls and number of transactions to daily
  my.endpoints <- endpoints(data.group1, "days")
  
  data.group1.daily <- period.apply(data.group1[,c(grep("pnl", names(data.group1)),
                                                   grep("ntrans", names(data.group1)))],
                                    INDEX = my.endpoints, 
                                    FUN = function(x) colSums(x, na.rm = TRUE))
  
  # Summarize the strategy for this quarter
  
  # Sharpe Ratio
  grossSR = mySR(x = data.group1.daily$pnl_gross.mr, scale = 252)
  netSR = mySR(x = data.group1.daily$pnl_net.mr, scale = 252)
  
  # Calmar Ratio
  grossCR = myCalmarRatio(x = data.group1.daily$pnl_gross.mr, scale = 252)
  netCR = myCalmarRatio(x = data.group1.daily$pnl_net.mr, scale = 252)
  
  # average number of transactions
  av.daily.ntrades = mean(data.group1.daily$ntransSP.mr + 
                            data.group1.daily$ntransNQ.mr, na.rm = TRUE)
  # P&L
  grossPnL = sum(data.group1.daily$pnl_gross.mr)
  netPnL = sum(data.group1.daily$pnl_net.mr)
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
    plot(cbind(cumsum(data.group1.daily$pnl_gross.mr),
               cumsum(data.group1.daily$pnl_net.mr)),
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