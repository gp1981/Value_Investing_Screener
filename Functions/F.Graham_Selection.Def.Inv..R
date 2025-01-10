## Author: Gabriele Picheo - 23 April 2020

# INPUT: ROC_EY
# OUTPUT: 

# Note: Selection of stocks based on the 7 criteria of Graham Intelligent Investor ch.13 - WIP

# Criteria
# 1) Adequate Size of the Enterprise: minimum 50m$ mktcap AND 100m$ sales
# 2) Sufficiently strong financial conditions: C.Asset/C.Liabilities > 2 AND Long.Term.Debt < (C.Asset-C.Liabolities)
# 3) Earnings Stability: Net.Income >0 for the last 10 years
# 4) Dividend Record: Dividend.per.share >0 for the last 20 years (max time)
# 5) Earning growth: avg3y(EPS)_Y10 / avg3y(EPS)_Y1 > 1.33
# 6) Moderate PE ratio : PE <15
# 7) Moderate ratio of Price to Asset: Price/Book.value * PE < 22.5 OR Price/Book.value <1.5


Graham_Selection.Def.Inv <- function(DF_all_FQ_G)
{

# 01 - Annualization of the quarterly result ------------------------------


  DF <- DF_all_FQ_G # %>% 
    # group_by(Ticker,year) %>% 
    # summarise(Revenue.Y = sum(as.numeric(Revenue)))
    # # Grouping by year and summarize Income, earnings,
  
  

# 02 - Criterion 1 --------------------------------------------------------


  DF1 <- DF %>% 
    filter(
      Market.Cap > as.numeric(50000000) & 
      Revenue > as.numeric(100000000)
      )
  

# 03 - Criterion 2 --------------------------------------------------------


  DF2 <- DF1 %>% 
     filter(
      as.numeric(Total.current.assets)/as.numeric(Total.current.liabilities) < 2  & 
        as.numeric(Long.term.debt) < (as.numeric(Total.current.assets) - as.numeric(Total.current.liabilities))
    )
  

# 04 - Criterion 3 --------------------------------------------------------


  # Net Income trailing average of 3 years >0
  Sum.DF2.Net.Income <- DF2 %>% 
    group_by(Ticker,year) %>% 
    summarise(Net.Income.Y = sum(as.numeric(Net.Income))) %>% 
    mutate(NI.Trailing.Avg.3y = rollmean(Net.Income.Y,k = 3, fill = NA,align = "right")) %>% 
    arrange(Ticker,desc(year))
  
  DF3 <- DF2 %>% 
    select(Ticker,date,quarter,year,DF.Stock.Price,Net.Income,everything())
  
  DF3 <- left_join(DF3,Sum.DF2.Net.Income)
  
  DF3 <- DF3 %>% 
    select(Ticker,date,quarter,year,DF.Stock.Price,Net.Income,NI.Trailing.Avg.3y,Net.Income.Y,everything())
  
  DF3 <- DF3 %>% 
      filter(is.na(NI.Trailing.Avg.3y) | as.numeric(NI.Trailing.Avg.3y) > 0 )
  

# 05 - Criterion 4 --------------------------------------------------------


  DF4 <- DF3 %>% 
    filter(Dividend.payments > 0)
  

# 06 - Criterion 5 --------------------------------------------------------


# 07 - Criterion 6 --------------------------------------------------------


# 08 - Criterion 7 --------------------------------------------------------


  return(DF)
}