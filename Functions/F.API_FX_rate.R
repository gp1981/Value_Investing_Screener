# Author: gp1981

# PURPOSE: Retrieve and compile a dataframe of FX rates to USD using the FinancialModelingPrep API.

# DISCLAIMER: This code is provided as is, without any guarantees or warranties. Use at your own risk.

# INPUT: API_Key – A valid API key string for accessing the FinancialModelingPrep endpoints.
# OUTPUT: A tibble with two columns: 'fx_symbol' (e.g. MYRUSD) and 'value' (numeric exchange rate), including USDUSD = 1.0.

API_FX_rate <- function(API_Key) {
  
  # Base URLs
  FX_list_url_path_base <- 'https://financialmodelingprep.com/stable/forex-list?apikey='
  FX_base <- 'https://financialmodelingprep.com/stable/quote-short?symbol='
  
  # Retrieve List of currencies & FX symbols
  FX_list_url <- paste0(FX_list_url_path_base,API_Key)
  FX_list_df <- fromJSON(FX_list_url)
  
  # USD Converted Currencies
  FX_list_df_USD <- FX_list_df %>% filter(toCurrency =="USD")
  total_cur <- length(FX_list_df_USD$symbol)
  

  # Fetch FX rates
  fx_rates <- list()
  for (i in seq_along(FX_list_df_USD$symbol)) {
    cur <- FX_list_df_USD$symbol[i]
    cat("Processing FX", cur, "-", round(i / total_cur * 100, 1), "% complete\n")
    fx_url <- paste0(FX_base, cur,'&apikey=', API_Key)
    
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
  
  # Create FX_rates dataframe
  fx_rates_df <- tibble(
    fx_symbol = names(fx_rates),
    value = unlist(fx_rates)
  )
  
  # Add 1.0 for USD
  fx_rates_df <- fx_rates_df %>% 
    add_row(fx_symbol = "USDUSD", value = 1.0)
 
   return(fx_rates_df)
}
  