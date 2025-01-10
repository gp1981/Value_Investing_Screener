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
      !is.na(as.numeric(cashAndShortTermInvestments)),
      !is.na(as.numeric(revenue)),
      !as.numeric(revenue)<=0
    ) %>%
    dplyr::summarise(
      median.cash_over_revenue = median(as.numeric(cashAndShortTermInvestments, na.rm = TRUE) / as.numeric(revenue, na.rm = TRUE), na.rm = TRUE),
      count.stocks.industry = n()
    )
  
  # Join the median cash data with the original data frame
  DF <- left_join(DF, median.cash.industry, by = c('year', 'industry'))
  
  # Calculate the excess of cash based on different conditions
  DF <- DF %>%
    mutate(
      Excess.Cash = case_when(
        as.numeric(cashAndShortTermInvestments) > as.numeric(cashAndCashEquivalents) ~ as.numeric(cashAndShortTermInvestments) - as.numeric(cashAndCashEquivalents) - (as.numeric(dividendsPaid) + as.numeric(commonStockRepurchased)),
        as.numeric(cashAndShortTermInvestments) <= as.numeric(cashAndCashEquivalents) & as.numeric(revenue) > 0 ~ as.numeric(revenue) * 0.05,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      Cash_ST.Industry.Benchmark = median.cash_over_revenue * as.numeric(revenue),
      Excess.Cash.Industry.Benchmark = as.numeric(cashAndShortTermInvestments, na.rm = TRUE) - as.numeric(Cash_ST.Industry.Benchmark, na.rm = TRUE)
    ) %>%
    mutate(
      Excess.Cash.2 = case_when(
        as.numeric(cashAndShortTermInvestments) > as.numeric(cashAndCashEquivalents) ~ Excess.Cash,
        Excess.Cash.Industry.Benchmark < as.numeric(cashAndShortTermInvestments) & Excess.Cash.Industry.Benchmark > 0 ~ Excess.Cash.Industry.Benchmark,
        as.numeric(revenue) > 0 ~ as.numeric(revenue) * 0.05,
        TRUE ~ 0
      )
    ) %>% 
    
    mutate(Excess.Cash = Excess.Cash.2)
  
  # Remove intermediate columns
  DF <- DF %>%
    select(-Cash_ST.Industry.Benchmark, -Excess.Cash.Industry.Benchmark, -median.cash_over_revenue, -Excess.Cash.2)
  
  return(DF)
}
