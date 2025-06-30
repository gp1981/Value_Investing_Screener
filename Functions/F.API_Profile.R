# Author: gp1981

# PURPOSE: Retrieve all the companies Profile from data available on FMP for all the companies in Stock_List_data

# DISCLAIMER: This code is provided as is, without any guarantees or warranties. Use at your own risk.

# INPUT: Stock_List_data - a data frame containing the list of stock tickers
#        API_Key - your API key for accessing the FMP API
#        

# OUTPUT: A list containing the Profile data for each company in Stock_List_data

API_Profile <- function(Stock_List_data, API_Key, countries) {
  
  # Base URLs
  API_Profile_path_base <- 'https://financialmodelingprep.com/stable/profile?symbol='
  FX_base <- 'https://financialmodelingprep.com/stable/quote-short?symbol='
  
  # Initialize variables
  Profile_list <- list()
  total_stocks <- length(Stock_List_data$Ticker)
  i <- 1
  
  # Step 1: Fetch exchange rates only for non-USD currencies
  unique_currencies <- unique(Stock_List_data$reportingCurrency)
  non_usd_currencies <- unique_currencies[unique_currencies != "USD"]
  
  # Fetch FX rates
  fx_rates <- list()
  for (cur in non_usd_currencies) {
    fx_url <- paste0(FX_base, cur, 'USD&apikey=', API_Key)
    tryCatch({
      fx_data <- fromJSON(fx_url)
      if (length(fx_data) > 0) {
        fx_rates[[cur]] <- as.numeric(fx_data$price)
      } else {
        fx_rates[[cur]] <- NA
      }
    }, error = function(e) {
      message(paste("Error fetching FX for", cur, ":", e$message))
      fx_rates[[cur]] <- NA
    })
  }
  
  # Add 1.0 for USD
  fx_rates[["USD"]] <- 1.0
  
  # Define a function to process each ticker
  process_ticker <- function(ticker, currency) {
    cat("Processing Profile", ticker, "-", round(i / total_stocks * 100, 1), "% complete\n")
    
    # Construct API URL for the current ticker
    API_Profile_path <- paste0(API_Profile_path_base, ticker, '&apikey=', API_Key)
    
    result <- list(
      Profile = NULL
    )
    
    tryCatch({
      # Retrieve Profile
      Stock_Profile_temp <- fromJSON(API_Profile_path)
      if (length(Stock_Profile_temp) > 0) {
        profile_df <- data.frame(Stock_Profile_temp)
        profile_df$reportingCurrency <- currency
        fx_rate <- fx_rates[[currency]]
        profile_df$fx_rate <- fx_rate
        profile_df$marketCap_LocalFX_Profile <- if (!is.null(profile_df$marketCap) && !is.na(fx_rate)) {
          profile_df$marketCap / fx_rate
        } else {
          NA
        }
        result$Profile <- profile_df
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
  
  # Use mapply to include currency info per ticker
  results <- mapply(function(t, c) process_ticker(t, c), 
                    Stock_List_data$Ticker, 
                    Stock_List_data$reportingCurrency, 
                    SIMPLIFY = FALSE)
  
  # Flatten the list of dataframes and combine them into one
  flattened_results <- lapply(results, function(x) x$Profile)
  combined_df <- bind_rows(flattened_results)
  
  # Rename column "symbol" to "Ticker" for consistency
  if ("symbol" %in% colnames(combined_df)) {
    combined_df <- combined_df %>% rename(Ticker = symbol)
  }
 
   return(combined_df)
}
  