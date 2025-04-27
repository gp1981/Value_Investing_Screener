# Author: gp1981
# Date: 24 Dec 2022
# Purpose: Calculate the excess of cash
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# INPUT: Reduced DF Dataframe financials (BS, IS, CF), KeyMetrics, Profile extracted from FMP
# OUTPUT: Excess of cash

Excess_Cash <- function(DF) {
  library(dplyr)
  
  # Calculate median cash over revenue for each year and industry
  median.cash.industry <- DF %>%
    group_by(year, industry) %>%
    filter(
      !is.na(cashAndShortTermInvestments),
      !is.na(revenue),
      revenue<=0
    ) %>%
    dplyr::summarise(
      median.cash_over_revenue = median(cashAndShortTermInvestments / revenue, na.rm = TRUE),
      count.stocks.industry = n()
    )
  
  # Join the median cash data with the original data frame
  DF <- left_join(DF, median.cash.industry, by = c('year', 'industry'))
  
  # Calculate the excess of cash based on different conditions
  DF <- DF %>%
    mutate(
      Excess.Cash = case_when(
        cashAndShortTermInvestments > cashAndCashEquivalents ~ cashAndShortTermInvestments - cashAndCashEquivalents - 
          (commonDividendsPaid + preferredDividendsPaid + commonStockRepurchased),
        cashAndShortTermInvestments <= cashAndCashEquivalents & revenue > 0 ~ revenue * 0.05,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      Cash_ST.Industry.Benchmark = median.cash_over_revenue * revenue,
      Excess.Cash.Industry.Benchmark = cashAndShortTermInvestments - Cash_ST.Industry.Benchmark
    ) %>%
    mutate(
      Excess.Cash.2 = case_when(
        cashAndShortTermInvestments > cashAndCashEquivalents ~ Excess.Cash,
        Excess.Cash.Industry.Benchmark < cashAndShortTermInvestments & Excess.Cash.Industry.Benchmark > 0 ~ Excess.Cash.Industry.Benchmark,
        revenue > 0 ~ revenue * 0.05,
        TRUE ~ 0
      )
    ) %>% 
    
    mutate(Excess.Cash = Excess.Cash.2)
  
  # Remove intermediate columns
  DF <- DF %>%
    select(-Cash_ST.Industry.Benchmark, -Excess.Cash.Industry.Benchmark, -median.cash_over_revenue, -Excess.Cash.2)
  
  return(DF)
}
