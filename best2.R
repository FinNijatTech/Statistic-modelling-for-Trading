library(xts)
library(chron)
library(TTR)
library(tseries)
library(knitr) # for nicely looking tables in html files
library(kableExtra) # for even more nicely looking tables in html files
library(quantmod) # for PnL graphs

# lets change the LC_TIME option to English
Sys.setlocale("LC_TIME", "English")

# mySR function
mySR <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} 

myCalmarRatio <- function(x, # x = series of returns
                          # scale parameter = Nt
                          scale) {
  scale * mean(coredata(x), na.rm = TRUE) / 
    maxdrawdown(cumsum(x))$maxdrawdown
  
} # end of definition


# lets define the system time zone as America/New_York (used in the data)
Sys.setenv(TZ = 'America/New_York')



for (selected_quarter in c("2021_Q1", "2021_Q2", "2021_Q3", "2021_Q4", 
                           "2022_Q1", "2022_Q2", "2022_Q3", "2022_Q4", 
                           "2023_Q1", "2023_Q2", "2023_Q3", "2023_Q4")) {
  
  message(selected_quarter)
  
  # loading the data for a selected quarter from a subdirectory "data""
  
  filename_ <- paste0("data/data2_", selected_quarter, ".RData")
  
  load(filename_)
  
  data.group2 <- get(paste0("data2_", selected_quarter))
  
  times_ <- substr(index(data.group2), 12, 19)
  
  # the following common assumptions were defined:
  
  # lets calculate EMAfast and EMAslow for all series
  data.group2$XAG_EMAfast <- EMA(na.locf(data.group2$XAG), 67)
  data.group2$XAG_EMAslow <- EMA(na.locf(data.group2$XAG), 118)
  data.group2$XAU_EMAfast <- EMA(na.locf(data.group2$XAU), 191)
  data.group2$XAU_EMAslow <- EMA(na.locf(data.group2$XAU), 570)
  
  # put missing value whenever the original price is missing
  data.group2$XAG_EMAfast[is.na(data.group2$XAG)] <- NA
  data.group2$XAG_EMAslow[is.na(data.group2$XAG)] <- NA
  data.group2$XAU_EMAfast[is.na(data.group2$XAU)] <- NA
  data.group2$XAU_EMAslow[is.na(data.group2$XAU)] <- NA
  
  # lets calculate the position for the MEAN-REVERTING strategy
  # for each asset separately
  # if fast MA(t-1) > slow MA(t-1) => pos(t) = -1 [short]
  # if fast MA(t-1) <= slow MA(t-1) => pos(t) = 1 [long]
  #  caution! this strategy is always in the market!
  
  data.group2$position.XAG.mr <- ifelse(lag.xts(data.group2$XAG_EMAfast) >
                                           lag.xts(data.group2$XAG_EMAslow),
                                         -1, 1)
  
  data.group2$position.XAU.mr <- ifelse(lag.xts(data.group2$XAU_EMAfast) >
                                           lag.xts(data.group2$XAU_EMAslow),
                                         -1, 1)
  
  
  # lets apply the remaining assumptions
  # - exit all positions 10 minutes before the session end, i.e. at 16:50
  # - do not trade within the first 10 minutes after the break (until 18:10)
  
  data.group2$position.XAG.mr[times(times_) > times("16:50:00") &
                                 times(times_) <= times("18:10:00")] <- 0
  
  data.group2$position.XAU.mr[times(times_) > times("16:50:00") &
                                 times(times_) <= times("18:10:00")] <- 0
  
  
  # lets also fill every missing position with the previous one
  data.group2$position.XAG.mr <- na.locf(data.group2$position.XAG.mr, na.rm = FALSE)
  data.group2$position.XAU.mr <- na.locf(data.group2$position.XAU.mr, na.rm = FALSE)
  
  
  # calculating gross pnl - remember to multiply by the point value !!!!
  data.group2$pnl_gross.XAU.mr <- data.group2$position.XAU.mr * diff.xts(data.group2$XAU) * 100
  data.group2$pnl_gross.XAG.mr <- data.group2$position.XAG.mr * diff.xts(data.group2$XAG) * 5000
  
  # number of transactions
  data.group2$ntrans.XAG.mr <- abs(diff.xts(data.group2$position.XAG.mr))
  data.group2$ntrans.XAG.mr[1] <- 0
  
  data.group2$ntrans.XAU.mr <- abs(diff.xts(data.group2$position.XAU.mr))
  data.group2$ntrans.XAU.mr[1] <- 0
  
  # net pnl
  data.group2$pnl_net.XAG.mr <- data.group2$pnl_gross.XAG.mr  -
    data.group2$ntrans.XAG.mr * 7 # 7$ per transaction
  
  data.group2$pnl_net.XAU.mr <- data.group2$pnl_gross.XAU.mr  -
    data.group2$ntrans.XAU.mr * 12 # 12$ per transaction
  
  
  # aggregate pnls and number of transactions to daily
  my.endpoints <- endpoints(data.group2, "days")
  
  data.group2.daily <- period.apply(data.group2[,c(grep("pnl", names(data.group2)),
                                                   grep("ntrans", names(data.group2)))],
                                    INDEX = my.endpoints, 
                                    FUN = function(x) colSums(x, na.rm = TRUE))
  
  # lets SUM gross and net pnls
  
  data.group2.daily$pnl_gross.mr <- 
    data.group2.daily$pnl_gross.XAU.mr +
    data.group2.daily$pnl_gross.XAG.mr
  
  data.group2.daily$pnl_net.mr <- 
    data.group2.daily$pnl_net.XAU.mr +
    data.group2.daily$pnl_net.XAG.mr
  
  # lets SUM number of transactions (with the same weights)
  
  data.group2.daily$ntrans.mr <- 
    data.group2.daily$ntrans.XAG.mr +
    data.group2.daily$ntrans.XAU.mr
  
  
  # summarize the strategy for this quarter
  
  # SR
  grossSR = mySR(x = data.group2.daily$pnl_gross.mr, scale = 252)
  netSR = mySR(x = data.group2.daily$pnl_net.mr, scale = 252)
  # CR
  grossCR = myCalmarRatio(x = data.group2.daily$pnl_gross.mr, scale = 252)
  netCR = myCalmarRatio(x = data.group2.daily$pnl_net.mr, scale = 252)
  
  # average number of transactions
  av.daily.ntrades = mean(data.group2.daily$ntrans.mr, 
                          na.rm = TRUE)
  # PnL
  grossPnL = sum(data.group2.daily$pnl_gross.mr)
  netPnL = sum(data.group2.daily$pnl_net.mr)
  
  # stat
  stat = netCR * max(0, log(abs(netPnL/1000)))
  
  # collecting all statistics for a particular quarter
  
  quarter_stats <- data.frame(quarter = selected_quarter,
                              assets.group = 2,
                              grossSR,
                              netSR,
                              grossCR,
                              netCR,
                              av.daily.ntrades,
                              grossPnL,
                              netPnL,
                              stat,
                              stringsAsFactors = FALSE
  )
  
  # collect summaries for all quarters
  if(!exists("quarter_stats.all.group2")) quarter_stats.all.group2 <- quarter_stats else
    quarter_stats.all.group2 <- rbind(quarter_stats.all.group2, quarter_stats)
  
  # create a plot of gros and net pnl and save it to png file
  
  png(filename = paste0("pnl_group2_", selected_quarter, ".png"),
      width = 1000, height = 600)
  print( # when plotting in a loop you have to use print()
    plot(cbind(cumsum(data.group2.daily$pnl_gross.mr),
               cumsum(data.group2.daily$pnl_net.mr)),
         multi.panel = FALSE,
         main = paste0("Gross and net PnL for asset group 2 \n quarter ", selected_quarter), 
         col = c("#377EB8", "#E41A1C"),
         major.ticks = "weeks", 
         grid.ticks.on = "weeks",
         grid.ticks.lty = 3,
         legend.loc = "topleft",
         cex = 1)
  )
  dev.off()
  
  # remove all unneeded objects for group 2
  rm(data.group2, my.endpoints, grossSR, netSR, av.daily.ntrades,
     grossPnL, netPnL, stat, quarter_stats, data.group2.daily)
  
  gc()
  
  
} # end of the loop

write.csv(quarter_stats.all.group2, 
          "quarter_stats.all.group2.csv",
          row.names = FALSE)

# Assuming quarter_stats.all.group1$netPnL is your vector
last_7_elements <- tail(quarter_stats.all.group2$netPnL, 7)
sum_last_7_elements <- sum(last_7_elements)

# Display the result
print(sum_last_7_elements)