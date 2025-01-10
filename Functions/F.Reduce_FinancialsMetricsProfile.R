# Author: Gabriele Picheo
# Date: 17 Apr. 2021
# Purpose: Reduce the list "FinancialsMetricsProfile" and remove columns with suffixes ".1", ".2", ".x", ".y" that are duplicates or wrong matches
# Disclaimer: This code is provided as-is without any guarantees or warranties. Use at your own risk.

library(dplyr)
library(tidyr)
library(lubridate)

Reduce_FinancialsMetricsProfile <- function(FinancialsMetricsProfile) {
  # Rename variable "CompanyName" of Stock List Data into "companyName"
  FinancialsMetricsProfile$Stock_List_data <- FinancialsMetricsProfile$Stock_List_data %>%
    select(companyName, everything())
  
  FinancialsMetricsProfile$IncomeStatement <- FinancialsMetricsProfile$IncomeStatement %>% 
    mutate(
      date = as.Date(date),
      fillingDate = as.Date(fillingDate),
      acceptedDate = as.Date(acceptedDate),
      calendarYear = as.integer(calendarYear)
      )
  
  FinancialsMetricsProfile$BalanceSheet <- FinancialsMetricsProfile$BalanceSheet %>% 
    mutate(
      date = as.Date(date),
      fillingDate = as.Date(fillingDate),
      acceptedDate = as.Date(acceptedDate),
      calendarYear = as.integer(calendarYear)
    )
  
  FinancialsMetricsProfile$CashFlow <- FinancialsMetricsProfile$CashFlow %>% 
    mutate(
      date = as.Date(date),
      fillingDate = as.Date(fillingDate),
      acceptedDate = as.Date(acceptedDate),
      calendarYear = as.integer(calendarYear)
    )
  
  FinancialsMetricsProfile$KeyMetrics <- FinancialsMetricsProfile$KeyMetrics %>% 
    mutate(
      date = as.Date(date),
      calendarYear = as.integer(calendarYear)
    )
  
  FinancialsMetricsProfile$Ratios <- FinancialsMetricsProfile$Ratios %>% 
    mutate(
      date = as.Date(date),
      calendarYear = as.integer(calendarYear)
    )
  
  FinancialsMetricsProfile$Shares_Float <- FinancialsMetricsProfile$Shares_Float %>% 
    mutate(
      date = as.Date(date),
    )
  
  FinancialsMetricsProfile$Stock_List_data <- FinancialsMetricsProfile$Stock_List_data %>% 
    mutate(
      ipoDate = as.Date(ipoDate),
      )
  
  DF_IS <- FinancialsMetricsProfile$IncomeStatement
  DF_BS <- FinancialsMetricsProfile$BalanceSheet
  DF_CF <- FinancialsMetricsProfile$CashFlow
  DF_KM_TTM <- FinancialsMetricsProfile$KeyMetrics_TTM
  DF_KM <- FinancialsMetricsProfile$KeyMetrics
  DF_Ratios_TTM <- FinancialsMetricsProfile$Ratios_TTM
  DF_Ratios <- FinancialsMetricsProfile$Ratios
  DF_Shares_Float <- FinancialsMetricsProfile$Shares_Float
  DF_ST <- FinancialsMetricsProfile$Stock_List_data
  
  # Combine the data frames by Ticker, year, month and drop duplicated columns
  # Add artificial row identifier to both DataFrames
  # DF_ST$row_id <- seq_len(nrow(Stock_List_data))
  # DF_KM_TTM$row_id <- seq_len(nrow(DF_KM_TTM))
  # DF_Ratios_TTM$row_id <- seq_len(nrow(DF_Ratios_TTM))

  DF <- DF_ST %>%  
    left_join(DF_Ratios_TTM, by = c("Ticker"))
  
  DF <- DF %>% 
    left_join(DF_KM_TTM, by = c("Ticker")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%  
    left_join(DF_BS, by = c("Ticker")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%
    left_join(DF_IS, by = c("date", "Ticker")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%
    left_join(DF_CF,  by = c("date", "Ticker")) %>% 
    filter(!is.na(freeCashFlow)) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%
    left_join(DF_KM, by = c("date", "Ticker")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>%
    left_join(DF_Ratios, by = c("date", "Ticker")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix

  DF <- DF %>%
    left_join(DF_Shares_Float, by = c("date", "Ticker")) %>% 
    select(-ends_with(".y")) %>%  # Remove .y columns
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))  # Rename .x columns by removing the suffix
  
  DF <- DF %>% 
    mutate(across(where(is.integer), as.numeric))
  
  DF <- DF %>% 
    mutate(outstandingShares = as.numeric(outstandingShares))
  
  DF <- DF %>% 
    mutate(outstandingShares = ifelse(is.na(outstandingShares), weightedAverageShsOutDil, outstandingShares))
  
  return(DF)
}
