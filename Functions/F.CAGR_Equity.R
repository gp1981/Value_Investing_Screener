# Author: Gabriele Picheo - 22 Nov. 2024

# INPUT: DF with all fundamentals and maintenance capex
# OUTPUT: 

#   - Full Equity growth
#   - Ratio no. quarters FCF negative


CAGR_Equity <- function(df){
  
  # 01 - Calculation full Equity growth ------------------------------------------
  
  
  # Calculate full equity and CAGR
  df_cagr <- df %>%
    group_by(Ticker) %>% 
    arrange(date) %>% # Ensure data is sorted by date
    mutate(
      # Get the last (newest) values
      last_equity = last(totalStockholdersEquity),
      last_totalDebt = coalesce(last(totalDebt),0),
      
      # Get the first (oldest) values
      first_totalDebt = coalesce(first(totalDebt),0),
      
      
      # Calculate cumulative sums for required fields up to each row
      cum_dividends = cumsum(coalesce(commonDividendsPaid, 0) + coalesce(preferredDividendsPaid, 0)),
      cum_issued = cumsum(coalesce(commonStockIssuance, 0)),
      cum_repurchased = cumsum(coalesce(commonStockRepurchased, 0)),
      change_totalDebt = last_totalDebt - first_totalDebt,
      
      # Calculate full equity using the first value of totalStockholdersEquity
      full_equity = last_equity + (-1) * cum_dividends - cum_issued + (-1) * cum_repurchased - change_totalDebt
    ) %>%
    # Calculate CAGR
    mutate(
      first_full_equity = first(full_equity), # Full equity at the first quarter
      years_elapsed = as.numeric(difftime(date, first(date), units = "days")) / 365.25,
      CAGR.full.Equity = ifelse(years_elapsed > 0,
                    (full_equity / first_full_equity)^(1 / years_elapsed) - 1,
                    NA_real_) # Avoid divide-by-zero for the first quarter
    ) %>% 
    ungroup() %>% 
    arrange(desc(date))
  

  return(df_cagr)
}
