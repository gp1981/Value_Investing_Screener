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
      # Get the last (newest) value of totalStockholdersEquity
      last_equity = last(totalStockholdersEquity),
      
      # Calculate cumulative sums for required fields up to each row
      cum_dividends = cumsum(dividendsPaid),
      cum_issued = cumsum(commonStockIssued),
      cum_repurchased = cumsum(commonStockRepurchased),
      
      # Calculate full equity using the first value of totalStockholdersEquity
      full_equity = last_equity + (-1) * cum_dividends - cum_issued + (-1) * cum_repurchased
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
