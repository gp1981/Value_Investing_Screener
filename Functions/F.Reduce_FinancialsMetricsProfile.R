# Author: Gabriele Picheo
# Date: 17 Apr. 2021
# Purpose: Reduce the list "FinancialsMetricsProfile" and remove columns with suffixes ".1", ".2", ".x", ".y" that are duplicates or wrong matches
# Disclaimer: This code is provided as-is without any guarantees or warranties. Use at your own risk.

Reduce_FinancialsMetricsProfile <- function(FinancialsMetricsProfile) {
  
  # Helper function
  clean_joins <- function(df) {
    df %>%
      select(-matches("\\.y$")) %>%
      rename_with(~ gsub("\\.x$", "", .), matches("\\.x$"))
  }
  
  # Helper function to prioritize TTM values
  prioritize_TTM <- function(dynamic_value, ttm_value, row) {
    if_else(!is.na(ttm_value) & row == 1, ttm_value, dynamic_value)
  }
  
  # --- Convert dates properly ---
  FinancialsMetricsProfile <- lapply(FinancialsMetricsProfile, function(df) {
    if ("date" %in% names(df)) df$date <- as.Date(df$date)
    if ("filingDate" %in% names(df)) df$filingDate <- as.Date(df$filingDate)
    if ("acceptedDate" %in% names(df)) df$acceptedDate <- as.Date(df$acceptedDate)
    if ("fiscalYear" %in% names(df)) df$fiscalYear <- as.integer(df$fiscalYear)
    if ("ipoDate" %in% names(df)) df$ipoDate <- as.Date(df$ipoDate)
    return(df)
  })
  
  # --- Extract individual DataFrames ---
  DF_IS <- FinancialsMetricsProfile$IncomeStatement
  DF_BS <- FinancialsMetricsProfile$BalanceSheet
  DF_CF <- FinancialsMetricsProfile$CashFlow
  DF_KM_TTM <- FinancialsMetricsProfile$KeyMetrics_TTM
  DF_KM <- FinancialsMetricsProfile$KeyMetrics
  DF_Ratios_TTM <- FinancialsMetricsProfile$Ratios_TTM
  DF_Ratios <- FinancialsMetricsProfile$Ratios
  DF_Shares_Float <- FinancialsMetricsProfile$Shares_Float
  DF_EV <- FinancialsMetricsProfile$EV
  DF_Profile <- FinancialsMetricsProfile$Stock_List_data
  
  # --- Rename variables
  if (all(c("accountsReceivables", "inventory", "accountsPayables",
            "otherWorkingCapital","otherNonCashItems") %in% names(DF_CF))) {
    DF_CF <- DF_CF %>% rename(
      Change_accountsReceivables = accountsReceivables,
      Change_inventory = inventory,
      Change_accountsPayables = accountsPayables,
      Change_otherWorkingCapital = otherWorkingCapital,
      Change_otherNonCashItems = otherNonCashItems
    )
  }
  
  if ("date" %in% names(DF_Shares_Float)) {
    DF_Shares_Float <- DF_Shares_Float %>% rename(
      share_float_date = date
    )
  }
  
  if ("marketCap" %in% names(DF_Profile)) {
    DF_Profile <- DF_Profile %>% rename(
      marketCap_USD_Profile = marketCap
    )
  }
  
  if ("date" %in% names(DF_Shares_Float)) {
    DF_Shares_Float <- DF_Shares_Float %>% rename(
      share_float_date = date
    )
  }
  
  # --- Merge TTM values ---
  DF_TTM <- DF_Profile %>%
    left_join(DF_Ratios_TTM, by = intersect(names(DF_Ratios_TTM),names(DF_Profile)))
  
  DF_TTM <- DF_TTM %>% 
    left_join(DF_KM_TTM, by = intersect(names(DF_KM_TTM),names(DF_TTM)))
  
  # --- Merge historical data ---
  DF <- DF_BS %>% 
    left_join(DF_IS, by = intersect(names(DF_IS),names(DF_BS)))
  
  DF <- DF %>% 
    left_join(DF_CF, by = c("Ticker","date"))
  
  DF <- DF %>% 
    left_join(DF_KM, by = c("Ticker","date"))
  
  DF <- DF %>% 
    left_join(DF_EV, c("Ticker","date"))
  
  # --- Merge recent data ---
  DF <- DF %>% 
    left_join(DF_TTM, by = c("Ticker"))
  
  DF <- DF %>% 
    left_join(DF_Shares_Float, by = c("Ticker"))
  
  DF <- DF %>%
    left_join(DF_Ratios, by = c(intersect(names(DF),names(DF_Ratios))))
  
  # --- Clean up suffixes from joins ---
  clean_join_suffixes <- function(df) {
    df <- df %>% select(-matches("\\.y$"))
    
    cols_x <- names(df)[str_ends(names(df), "\\.x$")]
    clean_names <- str_remove(cols_x, "\\.x$")
    
    df <- df %>% select(-any_of(clean_names))
    df %>% rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))
  }
  DF <- clean_join_suffixes(DF)
  
  
  # --- Standardize numeric types ---
  DF <- DF %>% 
    mutate(across(where(is.integer), as.numeric)) %>%
    mutate(outstandingShares = as.numeric(outstandingShares)) %>%
    mutate(outstandingShares = if_else(is.na(outstandingShares), weightedAverageShsOutDil, outstandingShares))
  return(DF)
}
