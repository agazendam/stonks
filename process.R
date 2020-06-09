#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if(!is.na(args[1])) { #Skip if run locally
  install.packages("tidyquant")
  install.packages("googledrive")
  install.packages("bizdays")
}

library("tidyquant")
library("tidyverse")
library("googledrive")
library("bizdays")

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  tiingo_api_key(read_file(".secrets/tiingo_api_key.txt"))
} else {
  tiingo_api_key(args[1])
}

drive_auth(path = ".secrets/my-project-92901-skicka-8a920f029b4b.json")
drive_empty_trash()

print("Setting symbols....")
if(!is.na(args[1])) { #Skip if run locally
  drive_download("00_stocks", type = "csv", overwrite = TRUE, verbose = FALSE)
  symbols <- read_csv("00_stocks.csv") %>% select(Symbols) %>% drop_na() %>% distinct() %>% pull(Symbols)
  unlink("00_stocks.csv")
} else symbols <- c("ABT")

print("Setting date range...")
start <- as_date("2017-04-01") # 1 April 2017 seems to be the start of the minutely data on iex
end   <- as_date(Sys.Date()+1)

load_quantlib_calendars('UnitedStates/NYSE', from=start, to=end)

for (symbol in sample(symbols,length(symbols))) {
  print(symbol)

  print("Declaring variables...")
  tiingo.iex.day <- tibble()
  tiingo.iex.hour <- tibble()
  tiingo.iex.minute <- tibble()
  
  try(suppressWarnings(if(length(tq_get(symbol, get = "tiingo.iex")) > 1) {
    
    if(drive_find(paste0(symbol,".day.csv"), verbose = FALSE) %>% count() != 0) {
      print("Downloading daily CSV file from Google Drive...")
      try({
        drive_download(paste0(symbol,".day.csv"), overwrite = TRUE, verbose = FALSE)
        suppressMessages(tiingo.iex.day <- read_csv(paste0(symbol,".day.csv")))
        unlink(paste0(symbol,".day.csv"))
      }
      )
    }
    if (following(as_date(max(tiingo.iex.day$date)+days(1)), "QuantLib/UnitedStates/NYSE") < (end-1)) {
      print("Downloading daily data from Tiingo...")
      theDate <- max(start,as_date(max(tiingo.iex.day$date)))
      while (theDate < end)
      {
        print(paste0(theDate," to ",min(end,theDate %m+% months(60*24))))
        try(tiingo.iex.day <- bind_rows(tiingo.iex.day,tq_get(symbol, get = "tiingo", from = theDate, to = min(end,theDate %m+% months(60*24)), resample_frequency = "daily") %>% rename(adjClose = adjusted)))
        theDate <- theDate %m+% months(60*24)
      }
      if (!is_empty(tiingo.iex.day)) {
        print("Processing daily data...")
        tiingo.iex.day <- tiingo.iex.day %>%
          select(symbol, date, open, high, low, close, volume, adjOpen, adjHigh, adjLow, adjClose, adjVolume) %>%
          drop_na() %>%
          distinct()
        print("Writing daily CSV file...")
        write_csv(tiingo.iex.day,paste0(symbol,".day.csv"))
        print("Uploading daily CSV file to Google Drive...")
        drive_upload(paste0(symbol,".day.csv"), path = "tiingo/", overwrite = TRUE, verbose = FALSE)
        print("Delete local version of the daily CSV file...")
        unlink(paste0(symbol,".day.csv"))
      }
    } else print("Daily data already up to date...")
    
    if(drive_find(paste0(symbol,".hour.csv"), verbose = FALSE) %>% count() != 0) {
      print("Downloading hourly CSV file from Google Drive...")
      try({
        drive_download(paste0(symbol,".hour.csv"), overwrite = TRUE, verbose = FALSE)
        suppressMessages(tiingo.iex.hour <- read_csv(paste0(symbol,".hour.csv")))
        unlink(paste0(symbol,".hour.csv"))
      }
      )
    }
    if (following(as_date(max(tiingo.iex.hour$date)+days(1)), "QuantLib/UnitedStates/NYSE") < (end-1)) {
      print("Downloading hourly data from Tiingo...")
      theDate <- max(start,as_date(max(tiingo.iex.hour$date)))
      while (theDate < end)
      {
        print(paste0(theDate," to ",min(end,theDate %m+% months(60))))
        try(tiingo.iex.hour <- bind_rows(tiingo.iex.hour,tq_get(symbol, get = "tiingo.iex", from = theDate, to = min(end,theDate %m+% months(60)), resample_frequency = "1hour")))
        theDate <- theDate %m+% months(60)
      }
      if (!is_empty(tiingo.iex.hour)) {
        print("Processing hourly data...")
        suppressMessages(tiingo.iex.hour <- tiingo.iex.hour %>%
                           rename(datetime = date) %>%
                           mutate(date = as_date(datetime)) %>%
                           full_join(tiingo.iex.day %>% mutate(adjustment = adjClose/close) %>% mutate(date = as_date(date)) %>% select(date,symbol,adjustment)) %>%
                           mutate(adjOpen = open * adjustment) %>%
                           mutate(adjHigh = high * adjustment) %>%
                           mutate(adjLow = low * adjustment) %>%
                           mutate(adjClose = close * adjustment) %>%
                           select(-date) %>%
                           rename(date = datetime) %>%
                           select(symbol, date, open, high, low, close, adjOpen, adjHigh, adjLow, adjClose) %>%
                           drop_na() %>%
                           distinct())
        print("Writing hourly CSV file...")
        write_csv(tiingo.iex.hour,paste0(symbol,".hour.csv"))
        print("Uploading hourly CSV file to Google Drive...")
        drive_upload(paste0(symbol,".hour.csv"), path = "tiingo/", overwrite = TRUE, verbose = FALSE)
        print("Delete local version of the hourly CSV file...")
        unlink(paste0(symbol,".hour.csv"))
      }
    } else print("Hourly data already up to date...")
    
    if(drive_find(paste0(symbol,".minute.csv"), verbose = FALSE) %>% count() != 0) {
      print("Downloading minutely CSV file from Google Drive...")
      try({
        drive_download(paste0(symbol,".minute.csv"), overwrite = TRUE, verbose = FALSE)
        suppressMessages(tiingo.iex.minute <- read_csv(paste0(symbol,".minute.csv")))
        unlink(paste0(symbol,".minute.csv"))
      }
      )
    }
    if (following(as_date(max(tiingo.iex.minute$date)+days(1)), "QuantLib/UnitedStates/NYSE") < (end-1)) {
      print("Downloading minutely data from Tiingo...")
      theDate <- max(start,as_date(max(tiingo.iex.minute$date)))
      while (theDate < end)
      {
        print(paste0(theDate," to ",min(end,theDate %m+% months(1))))
        #try(tiingo.iex.minute <- bind_rows(tiingo.iex.minute,tq_get(symbol, get = "tiingo.iex", from = theDate, to = min(end,theDate %m+% months(1)), resample_frequency = "1min")))
        print("Minutely downloads commented out to temporarily conserve Tiingo API limits")
        theDate <- theDate %m+% months(1)
      }
      if (!is_empty(tiingo.iex.minute)) {
        print("Processing minutely data...")
        suppressMessages(tiingo.iex.minute <- tiingo.iex.minute %>%
                           rename(datetime = date) %>%
                           mutate(date = as_date(datetime)) %>%
                           full_join(tiingo.iex.day %>% mutate(adjustment = adjClose/close) %>% mutate(date = as_date(date)) %>% select(date,symbol,adjustment)) %>%
                           mutate(adjOpen = open * adjustment) %>%
                           mutate(adjHigh = high * adjustment) %>%
                           mutate(adjLow = low * adjustment) %>%
                           mutate(adjClose = close * adjustment) %>%
                           select(-date) %>%
                           rename(date = datetime) %>%
                           select(symbol, date, open, high, low, close, adjOpen, adjHigh, adjLow, adjClose) %>%
                           drop_na() %>%
                           distinct())
        print("Writing minutely CSV file...")
        write_csv(tiingo.iex.minute,paste0(symbol,".minute.csv"))
        print("Uploading minutely CSV file to Google Drive...")
        drive_upload(paste0(symbol,".minute.csv"), path = "tiingo/", overwrite = TRUE, verbose = FALSE)
        print("Delete local version of the minutely CSV file...")
        unlink(paste0(symbol,".minute.csv"))
      }
    } else print("Minutely data already up to date...")
  } else print("Tiingo API limits temporarily exceeded...")))
}

drive_empty_trash()