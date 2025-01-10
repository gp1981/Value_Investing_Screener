# Author: gp1981

# PURPOSE: Retrieve all the companies Profile from data available on FMP for all the companies in Stock_List_data

# DISCLAIMER: This code is provided as is, without any guarantees or warranties. Use at your own risk.

# INPUT: Stock_List_data - a data frame containing the list of stock tickers
#        API_Key - your API key for accessing the FMP API
#        

# OUTPUT: A list containing the Profile data for each company in Stock_List_data

API_Profile <- function(Stock_List_data, API_Key, period, period_limit) {
  
  # Base URL for API calls
  API_Profile_path_base <- 'https://financialmodelingprep.com/api/v3/profile/'
  
  # Initialize a list to store profile data
  Profile_list <- list()
  
  total_stocks <- length(Stock_List_data$Ticker)
  i <- 1
  
  # Define a function to process each ticker
  process_ticker <- function(ticker) {
    cat("Processing Profile", ticker, "-", round(i / total_stocks * 100, 1), "% complete\n")
    
    # Construct API URL for the current ticker
    API_Profile_path <- paste0(API_Profile_path_base, ticker, '?apikey=', API_Key)
    
    result <- list(
      Profile = NULL
    )
    
    tryCatch({
      # Retrieve Profile
      Stock_Profile_temp <- fromJSON(API_Profile_path)
      if (length(Stock_Profile_temp) > 0) {
        result$Profile <- data.frame(Stock_Profile_temp)
      }
    }, error = function(cond) {
      message(paste("API provided an error for this Ticker:", ticker))
      message("Here's the original error message:")
      message(cond)
    }, warning = function(cond) {
      message(paste("API provided a warning for this Ticker:", ticker))
      message("Here's the original warning message:")
      message(cond)
    })
    
    i <<- i + 1  # Update i in the global environment
    
    return(result)
  }
  
  # Use lapply to process all tickers
  results <- lapply(Stock_List_data$Ticker, process_ticker)
  
  # Flatten the list of dataframes and combine them into one
  flattened_results <- lapply(results, function(x) x$Profile)
  combined_df <- bind_rows(flattened_results)
  
  # Rename column "symbol" to "Ticker" for consistency
  if ("symbol" %in% colnames(combined_df)) {
    combined_df <- combined_df %>% rename(Ticker = symbol)
  }
  
  return(combined_df)
}
