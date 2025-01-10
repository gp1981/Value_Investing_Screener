# Author: gp1981

# PURPOSE: Retrieve all the Financials, Profile, and Key Metrics data available on FMP for all the companies in Stock_List_data

# DISCLAIMER: This code is provided as is, without any guarantees or warranties. Use at your own risk.

# INPUT: Stock_List_data - a data frame containing the list of stock tickers
#        API_Key - your API key for accessing the FMP API
#        period - the period for the financial statements ("quarter" or "annual")
#        period_limit - the number of financial statements to retrieve

# OUTPUT: A list containing the Income Statement, Balance Sheet, Cash Flow, Key Metrics, Profile data for each company in Stock_List_data

API_QFMP <- function(Stock_List_data,API_Key, period, period_limit) {
  
  # Create API URLs for various calls to collect Financial Statements
  API_IncomeStatement_path_base <- 'https://financialmodelingprep.com/api/v3/income-statement/'
  API_BalanceSheet_path_base <- 'https://financialmodelingprep.com/api/v3/balance-sheet-statement/'
  API_CashFlow_path_base <- 'https://financialmodelingprep.com/api/v3/cash-flow-statement/'
  API_KeyMetrics_TTM_path_base <- 'https://financialmodelingprep.com/api/v3/key-metrics-ttm/'
  API_KeyMetrics_path_base <- 'https://financialmodelingprep.com/api/v3/key-metrics/'
  API_Ratios_TTM_path_base <- 'https://financialmodelingprep.com/api/v3/ratios-ttm/'
  API_Ratios_path_base <- 'https://financialmodelingprep.com/api/v3/ratios/'
  API_Shares_Float <- 'https://financialmodelingprep.com/api/v4/historical/shares_float?symbol='
  
  if (period == "quarter") {
    API_IncomeStatement_path_suffix <- '?period='
    API_BalanceSheet_path_suffix <- '?period='
    API_CashFlow_path_suffix <- '?period='
    API_KeyMetrics_path_suffix <- '?period='
    API_Ratios_path_suffix <- '?period='
  } else {
    API_IncomeStatement_path_suffix <- ''
    API_BalanceSheet_path_suffix <- ''
    API_CashFlow_path_suffix <- ''
    API_KeyMetrics_path_suffix <- ''
    API_Ratios_path_suffix <- ''
  }
  
  # Initialize lists to store data
  IS_list <- list()
  BS_list <- list()
  CF_list <- list()
  KeyMetrics_list_TTM <- list()
  KeyMetrics_list <- list()
  Ratios_TTM <- list()
  Ratios <- list()
  Shares_Float <- list()
  
  total_stocks <- length(Stock_List_data$Ticker)
  i <- 1
  
  # Define a function to process each ticker
  process_ticker <- function(ticker) {
    cat("Processing", ticker, "-", round(i / total_stocks * 100, 1), "% complete\n")
    
    # Construct API URLs for the current ticker
    API_IncomeStatement_path <- paste0(API_IncomeStatement_path_base, ticker, API_IncomeStatement_path_suffix, period,  '&limit=', period_limit, '&apikey=', API_Key)
    API_BalanceSheet_path <- paste0(API_BalanceSheet_path_base, ticker, API_BalanceSheet_path_suffix, period, '&limit=', period_limit, '&apikey=', API_Key)
    API_CashFlow_path <- paste0(API_CashFlow_path_base, ticker, API_CashFlow_path_suffix, period, '&limit=', period_limit, '&apikey=', API_Key)
    API_KeyMetrics_TTM_path <- paste0(API_KeyMetrics_TTM_path_base, ticker, '?apikey=', API_Key)
    API_KeyMetrics_path <- paste0(API_KeyMetrics_path_base, ticker,API_KeyMetrics_path_suffix, period, '&limit=', period_limit, '&apikey=', API_Key)
    API_Ratios_TTM_path <- paste0(API_Ratios_TTM_path_base, ticker, '?apikey=', API_Key)
    API_Ratios_path <- paste0(API_Ratios_path_base, ticker, API_Ratios_path_suffix, period, '&limit=', period_limit, '&apikey=', API_Key)
    API_Shares_Float_path <- paste0(API_Shares_Float, ticker, '&apikey=', API_Key)
    
    result <- list(
      IS = NULL,
      BS = NULL,
      CF = NULL,
      KM_TTM = NULL,
      KM = NULL,
      Ratios_TTM = NULL,
      Ratios = NULL,
      Shares_Float = NULL
    )
    
    tryCatch({
      # Retrieve Income Statement
      Stock_IncomeStatement_temp <- fromJSON(API_IncomeStatement_path)
      if (length(Stock_IncomeStatement_temp) > 0) {
        result$IS <- data.frame(Stock_IncomeStatement_temp)
      }
      
      # Retrieve Balance Sheet
      Stock_BalanceSheet_temp <- fromJSON(API_BalanceSheet_path)
      if (length(Stock_BalanceSheet_temp) > 0) {
        result$BS <- data.frame(Stock_BalanceSheet_temp)
      }
      
      # Retrieve Cash Flow Statement
      Stock_CashFlow_temp <- fromJSON(API_CashFlow_path)
      if (length(Stock_CashFlow_temp) > 0) {
        result$CF <- data.frame(Stock_CashFlow_temp)
      }
      
      # Retrieve Key Metrics TTM
      Stock_KeyMetrics_temp_TTM <- fromJSON(API_KeyMetrics_TTM_path)
      if (length(Stock_KeyMetrics_temp_TTM) > 0) {
        result$KM_TTM <- data.frame(Stock_KeyMetrics_temp_TTM)
        result$KM_TTM$Ticker <- ticker  # Add ticker column
      }
      
      # Retrieve Key Metrics
      Stock_KeyMetrics_temp <- fromJSON(API_KeyMetrics_path)
      if (length(Stock_KeyMetrics_temp) > 0) {
        result$KM <- data.frame(Stock_KeyMetrics_temp)
      }
      
      # Retrieve Ratios TTM
      Stock_Ratios_temp_TTM <- fromJSON(API_Ratios_TTM_path)
      if (length(Stock_Ratios_temp_TTM) > 0) {
        result$Ratios_TTM <- data.frame(Stock_Ratios_temp_TTM)
        result$Ratios_TTM$Ticker <- ticker  # Add ticker column
      }
      
      # Retrieve Ratios
      Stock_Ratios_temp <- fromJSON(API_Ratios_path)
      if (length(Stock_Ratios_temp) > 0) {
        result$Ratios <- data.frame(Stock_Ratios_temp)
      }
      
      # Retrieve Shares Float
      Shares_Float_temp <- fromJSON(API_Shares_Float_path)
      if (length(Shares_Float_temp) > 0) {
        result$Shares_Float <- data.frame(Shares_Float_temp)
      }
      
    }, error = function(cond) {
      message(paste("API provided an error for this Ticker:", ticker))  # Use ticker instead of Ticker
      message("Here's the original error message:")
      message(cond)
    }, warning = function(cond) {
      message(paste("API provided a warning for this Ticker:", ticker))  # Use ticker instead of Ticker
      message("Here's the original warning message:")
      message(cond)
    })
    
    i <<- i + 1
    
    return(result)
  }
  

  
  # Use lapply to process all tickers
  results <- lapply(Stock_List_data$Ticker, process_ticker)
  
  # Combine all dataframes
  IS <- bind_rows(lapply(results, function(x) x$IS))
  BS <- bind_rows(lapply(results, function(x) x$BS))
  CF <- bind_rows(lapply(results, function(x) x$CF))
  KeyMetrics_TTM <- bind_rows(lapply(results, function(x) x$KM_TTM))
  KeyMetrics <- bind_rows(lapply(results, function(x) x$KM))
  Ratios_TTM  <- bind_rows(lapply(results, function(x) x$Ratios_TTM))
  Ratios  <- bind_rows(lapply(results, function(x) x$Ratios))
  Shares_Float  <- bind_rows(lapply(results, function(x) x$Shares_Float))
  
  # Rename column "symbol" to "Ticker" for consistency
  if ("symbol" %in% colnames(IS)) {
    IS <- IS %>% rename(Ticker = symbol)
  }
  if ("symbol" %in% colnames(BS)) {
    BS <- BS %>% rename(Ticker = symbol)
  }
  if ("symbol" %in% colnames(CF)) {
    CF <- CF %>% rename(Ticker = symbol)
  }
  if ("symbol" %in% colnames(KeyMetrics_TTM)) {
    KeyMetrics_TTM <- KeyMetrics_TTM %>% rename(Ticker = symbol)
  }
  if ("symbol" %in% colnames(KeyMetrics)) {
    KeyMetrics <- KeyMetrics %>% rename(Ticker = symbol)
  }
  if ("symbol" %in% colnames(Ratios_TTM)) {
    Ratios_TTM <- Ratios_TTM %>% rename(Ticker = symbol)
  }
  if ("symbol" %in% colnames(Ratios)) {
    Ratios <- Ratios %>% rename(Ticker = symbol)
  }
  if ("symbol" %in% colnames(Shares_Float)) {
    Shares_Float <- Shares_Float %>% rename(Ticker = symbol)
  }
  
  FinancialsMetricsProfile <- list(
    IncomeStatement = IS,
    BalanceSheet = BS,
    CashFlow = CF,
    KeyMetrics_TTM = KeyMetrics_TTM,
    KeyMetrics = KeyMetrics,
    Ratios_TTM = Ratios_TTM,
    Ratios = Ratios,
    Shares_Float = Shares_Float,
    Stock_List_data = Stock_List_data
  )
  
  return(FinancialsMetricsProfile)
}
