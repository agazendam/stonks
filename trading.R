#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if(!is.na(args[1])) { #Skip if run locally
  install.packages("tidyverse")
  install.packages("bizdays")
  install.packages("lubridate")
  install.packages("tictoc")
  install.packages("googledrive")
}

#library(rsconnect)
#library(shiny)
#library(shinydashboard)
#library(plotly)
#library(htmlwidgets)
library(tidyverse)
#library(RQuantLib)
library(bizdays)
library(lubridate)
#library(tidyquant)
library(tictoc)
library(googledrive)

drive_auth(path = ".secrets/my-project-92901-skicka-8a920f029b4b.json")
drive_empty_trash()

print("Setting symbols...")
if(!is.na(args[1])) { #Skip if run locally
  drive_download("00_stocks", type = "csv", overwrite = TRUE, verbose = FALSE)
  symbols <- read_csv("00_stocks.csv") %>% select(Symbols) %>% drop_na() %>% distinct() %>% pull(Symbols)
  unlink("00_stocks.csv")
} else symbols <- c("UDOW")

symbols <- c("UDOW")

print("Setting date range....")
#start_date <- as_date("2020-01-01")
start_date <- as_date("2010-01-01")#"2017-01-01"
#end_date <- as_date("2020-01-01")
end_date <- as_date(today())

#trailing_stop <- 0.08
#limit <- 0.3
#buy_trigger <- 0.02
trailing_stop_min <- 0.15#0
trailing_stop_max <- 0.2
trailing_stop_step <- 0.025#0.01
limit_min <- 0.25#0
limit_max <- 0.4
limit_step <- 0.025#0.01
buy_trigger_min <- 0.0275#0
buy_trigger_max <- 0.03
buy_trigger_step <- 0.0025#0.001
window_size <- 1000000#3000

tic()
for (symbol in symbols) {
  print(symbol)
  
  drive_download(paste0(symbol,".day.csv"), overwrite = TRUE, verbose = FALSE)
  daily_all <- read_csv(paste0(symbol,".day.csv"))
  unlink(paste0(symbol,".day.csv"))
  
  # Comment this out in production
  daily <- daily_all %>%
    filter(date >= start_date & date <= end_date)
  
  starting_equity <- 100000
  cash <- 0
  
  results <- tibble(
    trailing_stop = numeric(),
    limit = numeric(),
    buy_trigger = numeric(),
    symbol = character(),
    date = as_date(now()),
    alpha = numeric(),
    buy_and_hold = numeric()
    #drawdown = numeric()
  )
  
  counter = 0
  total = ((trailing_stop_max-trailing_stop_min)/trailing_stop_step+1)*((limit_max-limit_min)/limit_step+1)*((buy_trigger_max-buy_trigger_min)/buy_trigger_step+1)
  
  for (trailing_stop in seq(trailing_stop_min, trailing_stop_max, trailing_stop_step)) {
    for (limit in seq(limit_min, limit_max, limit_step)) {
      for (buy_trigger in seq(buy_trigger_min, buy_trigger_max, buy_trigger_step)) {
        counter = counter + 1
        #print(paste0("trailing_stop=",trailing_stop_min,"/",trailing_stop,"/",trailing_stop_max,"; limit=",limit_min,"/",limit,"/",limit_max,"; buy_trigger=",buy_trigger_min,"/",buy_trigger,"/",buy_trigger_max))
        #print(paste0("Percentage complete = ",((trailing_stop-trailing_stop_min)/(trailing_stop_max-trailing_stop_min))*((limit-limit_min)/(limit_max-limit_min))*((buy_trigger-buy_trigger_min)/(buy_trigger_max-buy_trigger_min))*100,"%"))
        print(paste0("Percentage complete = ",round((counter-1)/total*100,digits=2),"%"))
        
        trading <- tibble(
          buy_signal = numeric(),
          sell_limit_signal = numeric(),
          sell_stop_signal = numeric(),
          buy_units = numeric(),
          sell_units = numeric(),
          buy_value = numeric(),
          sell_value = numeric(),
          cash_start = numeric(),
          cash_end = numeric(),
          start_units = numeric(),
          end_units = numeric(),
          equity_start = numeric(),
          equity_end = numeric(),
          highest = numeric(),
          lowest = numeric(),
          gain = numeric()
        )
        
        # equity_start <- vector("double", nrow(daily))
        # cash_start <- vector("double", nrow(daily))
        # start_units <- vector("double", nrow(daily))
        # buy_units <- vector("double", nrow(daily))
        # buy_value <- vector("double", nrow(daily))
        # sell_units <- vector("double", nrow(daily))
        # sell_value <- vector("double", nrow(daily))
        # end_units <- vector("double", nrow(daily))
        # cash_end <- vector("double", nrow(daily))
        # equity_end <- vector("double", nrow(daily))
        # lowest <- vector("double", nrow(daily))
        # highest <- vector("double", nrow(daily))
        # buy_signal <- vector("double", nrow(daily))
        # sell_stop_signal <- vector("double", nrow(daily))
        # sell_limit_signal <- vector("double", nrow(daily))
        # gain <- vector("double", nrow(daily))
        drawdown <- vector("double", nrow(daily))
        buy_and_hold <- vector("double", nrow(daily))
        
        trading[1,] <- 0
        
        trading[1,"equity_start"] <- starting_equity
        trading[1,"cash_start"] <- starting_equity
        trading[1,"cash_end"] <- trading[1,"cash_start"] - (trading[1,"buy_signal"] * trading[1,"buy_value"] * trading[1,"buy_units"]) + ((trading[1,"sell_limit_signal"] + trading[1,"sell_stop_signal"]) * trading[1,"sell_units"] * trading[1,"sell_value"])
        trading[1,"equity_end"] <- trading[1,"cash_end"] + (trading[1,"end_units"] * daily[1,]$adjClose)
        trading[1,"lowest"] <- daily[1,]$adjClose
        trading[1,"highest"] <- daily[1,]$adjClose
        trading[1,"gain"] <- 1
        buy_and_hold[1] <- 1
        
        # trading[1,"buy_signal"] <- 0
        # trading[1,"sell_stop_signal"] <- 0
        # trading[1,"sell_limit_signal"] <- 0
        # trading[1,"end_units"] <- 0
        # trading[1,"buy_units"] <- 0
        # trading[1,"buy_value"] <- 0
        # trading[1,"lowest"] <- 0
        # trading[1,"highest"] <- 0
        
        for (i in 2:nrow(daily)) {
          trading[i,] <- 0
          # trading[i,"buy_signal"] <- 0
          # trading[i,"sell_stop_signal"] <- 0
          # trading[i,"sell_limit_signal"] <- 0
          # trading[i,"end_units"] <- 0
          # trading[1,"buy_units"] <- 0
          # trading[i,"buy_value"] <- 0
          # trading[i,"lowest"] <- 0
          # trading[i,"highest"] <- 0
          
          trading[i,"equity_start"] <- trading[i-1,"equity_end"]
          trading[i,"cash_start"] <- trading[i-1,"cash_end"]
          trading[i,"start_units"] <- trading[i-1,"end_units"]
          
          if((trading[[i-1,"end_units"]] == 0) & (daily[i,]$adjHigh > (1+buy_trigger) * trading[[i-1,"lowest"]])) {
            trading[i,"buy_signal"] <- 1
          }
          
          if((trading[[i-1,"end_units"]] != 0) & (daily[i,]$adjLow < (1-trailing_stop) * trading[[i-1,"highest"]])) {
            trading[i,"sell_stop_signal"] <- 1
          }
          
          if(trading[[i,"sell_stop_signal"]] == 0) {
            if((trading[[i-1,"end_units"]] != 0) & (daily[i,]$adjHigh > (1+limit) * trading[[i-1,"buy_value"]])) {
              trading[[i,"sell_limit_signal"]] <- 1
            }
          }
          
          if(trading[[i,"buy_signal"]] == 1) {
            trading[[i,"buy_value"]] <- max(daily[i,]$adjOpen,(1+buy_trigger) * trading[[i-1,"lowest"]])
          }
          else {
            trading[[i,"buy_value"]] <- trading[[i-1,"buy_value"]]
          }
          
          if(trading[[i,"buy_signal"]] == 1) {
            trading[[i,"buy_units"]] <- trunc(trading[[i,"cash_start"]]/trading[[i,"buy_value"]])
          }
          
          if(trading[[i,"sell_stop_signal"]] == 1 | trading[[i,"sell_limit_signal"]] == 1) {
            trading[[i,"sell_units"]] <- trading[[i-1,"end_units"]]
          }
          
          if(trading[[i,"sell_limit_signal"]] == 1) {
            trading[[i,"sell_value"]] <- min(daily[i,]$adjOpen,(1+limit) * trading[[i-1,"buy_value"]])
          }
          else {
            if(trading[[i,"sell_stop_signal"]] == 1) {
              trading[[i,"sell_value"]] <- min(daily[i,]$adjOpen,(1-trailing_stop) * trading[[i-1,"highest"]])
            }
            else {
              trading[[i,"sell_value"]] <- trading[[i-1,"sell_value"]]
            }
          }
          
          trading[[i,"end_units"]] <- trading[[i,"start_units"]] + trading[[i,"buy_units"]] - trading[[i,"sell_units"]]
          trading[[i,"cash_end"]] <- trading[[i,"cash_start"]] - (trading[[i,"buy_signal"]] * trading[[i,"buy_value"]] * trading[[i,"buy_units"]]) + ((trading[[i,"sell_limit_signal"]] + trading[[i,"sell_stop_signal"]]) * trading[[i,"sell_units"]] * trading[[i,"sell_value"]])
          trading[[i,"equity_end"]] <- trading[[i,"cash_end"]] + (trading[[i,"end_units"]] * daily[i,]$adjClose)
          
          if(trading[[i,"sell_limit_signal"]] == 1 | trading[[i,"sell_stop_signal"]] == 1) {
            trading[[i,"lowest"]] <- daily[i,]$adjLow
          }
          else if(daily[i,]$adjLow < trading[[i-1,"lowest"]]) {
            trading[[i,"lowest"]] <- daily[i,]$adjLow
          }
          else {
            trading[[i,"lowest"]] <- trading[[i-1,"lowest"]]
          }
          
          if(trading[[i,"buy_signal"]] == 1) {
            trading[[i,"highest"]] <- daily[i,]$adjHigh
          }
          else if(daily[i,]$adjHigh > trading[[i-1,"highest"]]) {
            trading[[i,"highest"]] <- daily[i,]$adjHigh
          }
          else {
            trading[[i,"highest"]] <- trading[[i-1,"highest"]]
          }
          
          if(i <= window_size) {
            window_start <- 1
          }
          else {
            window_start <- i - window_size + 1
          }
          
          trading[[i,"gain"]] <- trading[[i,"equity_end"]] / trading[[window_start,"equity_end"]]
          #drawdown[i] <- (trading[[i,"gain"]]-(max(trading[window_start:i,"gain"])))/(max(trading[window_start:i,"gain"]))
          buy_and_hold[i] <- daily[i,]$adjClose / daily[window_start,]$adjClose
        }
        
        results <- results %>% add_row(
          #symbol = daily$symbol,
          symbol = symbol,
          date = daily$date,
          trailing_stop = trailing_stop,
          limit = limit,
          buy_trigger = buy_trigger,
          alpha = trading$gain,
          buy_and_hold = buy_and_hold)
          #,
          #drawdown = drawdown)
      }
    }
  }
  print("Writing trading CSV file...")
  write_csv(results,paste0(symbol,".trading.csv"))
  print("Uploading trading CSV file to Google Drive...")
  drive_upload(paste0(symbol,".trading.csv"), path = "tiingo/", overwrite = TRUE, verbose = FALSE)
  print("Delete local version of the trading CSV file...")
  unlink(paste0(symbol,".trading.csv"))
  print("Emptying trash on Google Drive...")
}

drive_empty_trash()

toc()
