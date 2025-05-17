# Author: Gabriele Picheo - 22 Nov. 2024

# INPUT: DF with all fundamentals and maintenance capex
# OUTPUT: 

#   - Full Equity growth
#   - Ratio no. quarters FCF negative


FCF_Negative <- function(df){
  
  # 01 - Calculation quarters with negative FCF over total no. quarters -----------
  
  
  df <- df %>%
    group_by(Ticker) %>% 
    arrange(date) %>%  # Ensure data is sorted by date from oldest to latest
    mutate(
      # Cumulative count of negative free cash flow quarters
      cumulative_negative_fc_quarters = cumsum(ifelse(is.na(freeCashFlow), FALSE, freeCashFlow < 0)),
      
      
      # Cumulative count of total quarters (1, 2, 3, ...)
      cumulative_total_quarters = row_number(),
      
      # Calculate the ratio of negative quarters to total quarters
      no.quarters.FCF_negative_ratio = cumulative_negative_fc_quarters / cumulative_total_quarters
    )  %>% 
    arrange(desc(date)) %>% 
    ungroup()
  
  # Prepare output
  df <- df %>% 
    select(
      Ticker, date, price,
      Earning.Power.per.Share.TTM,
      Owner.Earnings.Buffet.per.Share.TTM,
      Owner.Earnings.Buffet.IGVI.per.Share.TTM,
      Owner.Earnings.IGVI.per.Share.TTM,
      CAGR.full.Equity,
      no.quarters.FCF_negative_ratio,
      everything()
    )
  return(df)
}
