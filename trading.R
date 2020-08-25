#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#if(!is.na(args[1])) { #Skip if run locally
  install.packages("tidyverse")
  install.packages("bizdays")
  install.packages("lubridate")
  install.packages("tictoc")
  install.packages("googledrive")
#}

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
} else symbols <- c("TQQQ")

print("Setting date range....")
#start_date <- as_date("2020-01-01")
start_date <- as_date("2010-01-01")#"2017-01-01"
#end_date <- as_date("2020-01-01")
end_date <- as_date(today())

#trailing_stop <- 0.08
#limit <- 0.3
#buy_trigger <- 0.02
trailing_stop_min <- 0#0.15
trailing_stop_max <- 0.2
trailing_stop_step <- 0.01#0.025
limit_min <- 0#0.25
limit_max <- 0.4
limit_step <- 0.01#0.025
buy_trigger_min <- 0#0.0275
buy_trigger_max <- 0.03
buy_trigger_step <- 0.001#0.0025
window_size <- 1000000#3000

#tic()
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
    drawdown = numeric()
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
        
        equity_start <- vector("double", nrow(daily))
        cash_start <- vector("double", nrow(daily))
        start_units <- vector("double", nrow(daily))
        buy_units <- vector("double", nrow(daily))
        buy_value <- vector("double", nrow(daily))
        sell_units <- vector("double", nrow(daily))
        sell_value <- vector("double", nrow(daily))
        end_units <- vector("double", nrow(daily))
        cash_end <- vector("double", nrow(daily))
        equity_end <- vector("double", nrow(daily))
        lowest <- vector("double", nrow(daily))
        highest <- vector("double", nrow(daily))
        buy_signal <- vector("double", nrow(daily))
        sell_stop_signal <- vector("double", nrow(daily))
        sell_limit_signal <- vector("double", nrow(daily))
        gain <- vector("double", nrow(daily))
        drawdown <- vector("double", nrow(daily))
        buy_and_hold <- vector("double", nrow(daily))
        
        equity_start[1] <- starting_equity
        cash_start[1] <- starting_equity
        cash_end[1] <- cash_start[1] - (buy_signal[1] * buy_value[1] * buy_units[1]) + ((sell_limit_signal[1] + sell_stop_signal[1]) * sell_units[1] * sell_value[1])
        equity_end[1] <- cash_end[1] + (end_units[1] * daily[1,]$adjClose)
        lowest[1] <- daily[1,]$adjClose
        highest[1] <- daily[1,]$adjClose
        gain[1] <- 1
        buy_and_hold[1] <- 1
        
        for (i in 2:nrow(daily)) {
          equity_start[i] <- equity_end[i-1]
          cash_start[i] <- cash_end[i-1]
          start_units[i] <- end_units[i-1]
          
          if((end_units[i-1] == 0) & (daily[i,]$adjHigh > (1+buy_trigger) * lowest[i-1])) {
            buy_signal[i] <- 1
          }
          
          if((end_units[i-1] != 0) & (daily[i,]$adjLow < (1-trailing_stop) * highest[i-1])) {
            sell_stop_signal[i] <- 1
          }
          
          if(sell_stop_signal[i] == 0) {
            if((end_units[i-1] != 0) & (daily[i,]$adjHigh > (1+limit) * buy_value[i-1])) {
              sell_limit_signal[i] <- 1
            }
          }
          
          if(buy_signal[i] == 1) {
            buy_value[i] <- max(daily[i,]$adjOpen,(1+buy_trigger) * lowest[i-1])
          }
          else {
            buy_value[i] <- buy_value[i-1]
          }
          
          if(buy_signal[i] == 1) {
            buy_units[i] <- trunc(cash_start[i]/buy_value[i])
          }
          
          if(sell_stop_signal[i] == 1 | sell_limit_signal[i] == 1) {
            sell_units[i] <- end_units[i-1]
          }
          
          if(sell_limit_signal[i] == 1) {
            sell_value[i] <- min(daily[i,]$adjOpen,(1+limit) * buy_value[i-1])
          }
          else {
            if(sell_stop_signal[i] == 1) {
              sell_value[i] <- min(daily[i,]$adjOpen,(1-trailing_stop) * highest[i-1])
            }
            else {
              sell_value[i] <- sell_value[i-1]
            }
          }
          
          end_units[i] <- start_units[i] + buy_units[i] - sell_units[i]
          cash_end[i] <- cash_start[i] - (buy_signal[i] * buy_value[i] * buy_units[i]) + ((sell_limit_signal[i] + sell_stop_signal[i]) * sell_units[i] * sell_value[i])
          equity_end[i] <- cash_end[i] + (end_units[i] * daily[i,]$adjClose)
          
          if(sell_limit_signal[i] == 1 | sell_stop_signal[i] == 1) {
            lowest[i] <- daily[i,]$adjLow
          }
          else if(daily[i,]$adjLow < lowest[i-1]) {
            lowest[i] <- daily[i,]$adjLow
          }
          else {
            lowest[i] <- lowest[i-1]
          }
          
          if(buy_signal[i] == 1) {
            highest[i] <- daily[i,]$adjHigh
          }
          else if(daily[i,]$adjHigh > highest[i-1]) {
            highest[i] <- daily[i,]$adjHigh
          }
          else {
            highest[i] <- highest[i-1]
          }
          
          if(i <= window_size) {
            window_start <- 1
          }
          else {
            window_start <- i - window_size + 1
          }
          
          gain[i] <- equity_end[i] / equity_end[window_start]
          drawdown[i] <- (gain[i]-(max(gain[window_start:i])))/(max(gain[window_start:i]))
          buy_and_hold[i] <- daily[i,]$adjClose / daily[window_start,]$adjClose
        }
        
        results <- results %>% add_row(
          #symbol = daily$symbol,
          symbol = symbol,
          date = daily$date,
          trailing_stop = trailing_stop,
          limit = limit,
          buy_trigger = buy_trigger,
          alpha = gain,
          drawdown = drawdown)
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

#toc()