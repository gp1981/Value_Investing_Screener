# Author: gp1981
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# INPUT: API_KEY
# OUTPUT: Create a dataframe of all tickers of the S&P securities and their history in the index

API_SP500_Hist <- function(API_Key) {
  library(httr)
  library(jsonlite)
  library(tidyverse)
  library(progress)
  library(dplyr)
  library(foreach)
  library(zoo)
  
  
  
  # 01 - Create API URL for historical S&P500 and current data
  API_SP500_Hist_path_base <- rep('https://financialmodelingprep.com/api/v3/historical/sp500_constituent?apikey=')
  API_SP500_Hist_path <- paste0(API_SP500_Hist_path_base, API_Key)
  
  API_SP500_path_base <- rep('https://financialmodelingprep.com/api/v3/sp500_constituent?apikey=')
  API_SP500_path <- paste0(API_SP500_path_base, API_Key)
  
  # 02 - Connect to FMP data and create DF_SP500_Hist dataframe
  DF_SP500_Hist <- fromJSON(API_SP500_Hist_path)
  DF_SP500 <- fromJSON(API_SP500_path)
  
  # Convert JSON data into data frames
  DF_SP500_Hist <- as.data.frame(DF_SP500_Hist)
  DF_SP500 <- as.data.frame(DF_SP500)
  
  # 03 - Manipulate SP500 data to recreate history
  
  # Rename column 'symbol' to 'Ticker' to be consistent with DF_SP500
  DF_SP500_Hist <- dplyr::rename(DF_SP500_Hist, Ticker = symbol)
  DF_SP500 <- dplyr::rename(DF_SP500, Ticker = symbol)
  
  # Convert date formats
  DF_SP500_Hist <- DF_SP500_Hist %>%
    mutate(dateAdded = mdy(dateAdded))
  
  DF_SP500 <- DF_SP500 %>%
    mutate(dateFirstAdded = as_date(dateFirstAdded, format = "%Y-%m-%d"),
           founded = as_date(founded, format = "%Y"))
  
  # Split historical S&P500 into two data frames by filtering empty values
  DF_SP500_Hist_Added <- DF_SP500_Hist %>% 
    filter(removedTicker == "")
  
  DF_SP500_Hist_Removed <- DF_SP500_Hist %>% 
    filter(addedSecurity == "")
  
  # Add today's date
  DF_SP500 <- DF_SP500 %>% 
    mutate(date = as_date(Sys.Date(), "%Y-%m")) %>% 
    select(date, everything())
  
  # Create a vector of dates of changes
  dateseq <- DF_SP500_Hist %>% 
    select(dateAdded) %>% 
    mutate(date = as_date(dateAdded, format = "%Y-%m-%d")) %>% 
    select(-dateAdded) %>% 
    distinct(date)
  
  # Initialize the full historical SP500 constituents
  DF_SP500_Full <- DF_SP500
  
  # Create progress bar
  pb <- progress_bar$new(format = "[:bar] :percent :elapsed", total = length(dateseq$date))
  
  # Loop through each month from 1990 - 2021
  for (i in 1:length(dateseq$date)) {
    # Update progress bar
    pb$tick()
    
    date_n <- dateseq[i, 1]
    
    # Identify securities that have been removed and added
    removed <- DF_SP500_Hist_Removed %>% 
      filter(date == date_n) %>% 
      mutate(date = as_date(date, format = "%Y-%m-%d"))
    
    added <- DF_SP500_Hist_Added %>% 
      filter(date == date_n) %>% 
      mutate(date = as_date(date, format = "%Y-%m-%d"))
    
    # Recreate SP500 for previous period: remove records that were added and vice versa
    DF_SP500_n_1 <- DF_SP500_Full %>% 
      filter(date == min(date)) %>% 
      select(-date) %>% 
      mutate(date = date_n) %>% 
      select(date, everything()) %>% 
      anti_join(added, by = "Ticker") %>% 
      bind_rows(removed) %>% 
      select(-(8:14))
    
    DF_SP500_Full <- DF_SP500_Full %>% 
      bind_rows(DF_SP500_n_1)
  }
  
  DF <- DF_SP500_Full
  return(DF)
}
