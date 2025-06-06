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
      last_equity = last(
        coalesce(totalStockholdersEquity, lag(totalStockholdersEquity, 1))
      ),
      
      # Cumulative financial items
      cum_dividends = cumsum(coalesce(commonDividendsPaid, 0) + coalesce(preferredDividendsPaid, 0)),
      cum_issued = cumsum(coalesce(commonStockIssuance, 0)),
      cum_repurchased = cumsum(coalesce(commonStockRepurchased, 0)),
      
      # Calculate change in TotalDebt
      totalDebt_interp = na.approx(totalDebt, x = date, na.rm = FALSE),
      change_totalDebt = totalDebt_interp - lag(totalDebt_interp,1),
      cum_change_totalDebt = cumsum(coalesce(change_totalDebt, 0)),
      
      # Calculate full equity using the first value of totalStockholdersEquity
      full_equity = totalStockholdersEquity + (-1) * cum_dividends - cum_issued + (-1) * cum_repurchased - 
        cum_change_totalDebt,
      full_equity_noDebt = totalStockholdersEquity + (-1) * cum_dividends - cum_issued + (-1) * cum_repurchased 
    ) %>% 
    ungroup()
  
  # Calculate CAGR
  df_cagr <- df_cagr %>%
    group_by(Ticker) %>% 
    arrange(date) %>% # Ensure data is sorted by date  
    mutate(
      first_totalStockholdersEquity = first(totalStockholdersEquity), # Full equity at the first quarter
      years_elapsed = as.numeric(difftime(date, first(date), units = "days")) / 365.25,
      CAGR.full.Equity = ifelse(years_elapsed > 0,
                                (full_equity / first_totalStockholdersEquity)^(1 / years_elapsed) - 1,
                                NA_real_), # Avoid divide-by-zero for the first quarter
      CAGR.full.Equity_noDebt = ifelse(years_elapsed > 0,
                                       (full_equity_noDebt / first_totalStockholdersEquity)^(1 / years_elapsed) - 1,
                                       NA_real_) # Avoid divide-by-zero for the first quarter
      
    ) %>% 
    select(-totalDebt_interp) %>% 
    ungroup() %>% 
    arrange(desc(date))
  
  
  return(df_cagr)
}
