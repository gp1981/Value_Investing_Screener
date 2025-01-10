# Author: Gabriele Picheo - 20 April 2019

# INPUT: DF_all_Year
# OUTPUT: Cumulative CAGR based on price calculated as EPS x PE

# Note: Provide a script to calculate 
# 1) Avg. Price = EPS x PE
# 2) Price change percentage
# 3) cumulative CAGR from first available year


# Requirements source the following functions: 
#   DF_all_year dataframe

Cumulative_CAGR <- function(DF_all_FQ_Historical)
{
# 01 - Calculation of Yearly Stock Price (indirect method from PE* --------
  
  
  DF.historical <- DF_all_FQ_Historical %>%
    mutate(Avg.Price.PExEPS = 
             as.numeric(PE.ratio) * as.numeric(EPS.Diluted))  %>%
             mutate(DF.Stock.Price= historical.adjClose)
    # mutate('Avg_Price_(MktCap/RevXShare)' = as.numeric(`Market Cap`) / (as.numeric(Revenue) / as.numeric(`Revenue per Share`))) %>% -> TO VALIDATE -> SIMILAR RESULTS
    # mutate('Avg_Price_(MktCap/NIXShare)' = as.numeric(`Market Cap`) / (as.numeric(`Net Income`) / as.numeric(`Net Income per Share`))) %>% -> TO VALIDATE -> SIMILAR RESULTS
    # mutate('Avg_Price' = mean(as.numeric(`Avg_Price_PExEPS`),as.numeric(`Avg_Price_(MktCap/RevXShare)`),as.numeric(`Avg_Price_(MktCap/NIXShare)`))) -> TO VALIDATE -> SIMILAR RESULTS
    # select(Ticker, date,Year,'Avg_Price_PExEPS','price_TODAY','Combined Ranking','Rank Earnings Yield','Rank ROC',everything())


  # Incorporate Stock price of Today in the last time_date
  # DF_all_Year <- DF_all_Year %>%
  #   arrange(Ticker,desc(date)) %>%
  #   group_by(Ticker) %>%
  #   mutate('Avg_Price_PExEPS' = ifelse(date >= date[[1]],price_TODAY,`Avg_Price_PExEPS`))
  
  # DF.historical <- DF.historical %>% 
  #   mutate(DF.Stock.Price = coalesce(historical.adjClose,Avg.Price.PExEPS))
  # DF.historical$DF.Stock.Price[DF.historical$DF.Stock.Price < 0] <- 0
  

# 02 - Calculate Price Change ---------------------------------------------


  DF.historical <- DF.historical %>%
    group_by(Ticker) %>% 
    arrange(Ticker,desc(date)) %>% 
    mutate(Price.Change = ifelse(DF.Stock.Price != 0, 
                                 as.numeric(DF.Stock.Price)/as.numeric(lead(DF.Stock.Price)),0)) %>%
    mutate(Price.Change = replace(Price.Change,is.na(Price.Change),1)) %>%   #  replace NA value with 1
    mutate(Price.Change = replace(Price.Change,is.nan(Price.Change),1)) %>%  #  replace NAN (Not A Number) value with 1
    mutate(Price.Change = replace(Price.Change,is.infinite(Price.Change),1)) #%>%    replace -Inf value with 1
    # select(Ticker, date,quarter,'Price_Change','Avg_Price_PExEPS')
  
 

# 03 - Calculate CAGR -----------------------------------------------------


  #---- Calculate cumprod of price change 
  # DF_all_FQ <- DF_all_FQ %>%
  #   group_by(Ticker) %>%
  #   arrange(Ticker,date) %>%  # to use cumfunction reorder from oldest to newest date
  #   filter(Price_Change != 0) %>%
  #   mutate(cumprod_Price = cumprod(as.numeric(`Price_Change`))) %>% # Calculate cumulative Product of price change used to calculate CAGR (see excel "CAGR_Example.xlsx")
  #   arrange(Ticker,desc(date)) # %>% reverse date from  newest to oldest date
  #   # select(Ticker, date,quarter,'cumprod_Price','Price_Change','Avg_Price_PExEPS'),'price_TODAY','Combined Ranking','Rank Earnings Yield','Rank ROC',everything())
  
  # for each year it is the CAGR from the first available year 
  DF.historical <- DF.historical %>%
    group_by(Ticker) %>%
    arrange(Ticker,desc(date)) %>%
    filter(Price.Change != 0.000000) %>%
    mutate(t = (date - last(date))/365) %>%
    mutate(CAGR = (as.numeric(DF.Stock.Price)/as.numeric(last(DF.Stock.Price)))^(1/as.numeric(t))-1) %>%
    select(Ticker,date,quarter,CAGR,Price.Change,DF.Stock.Price, everything())

  
  DF.historical <- DF.historical %>% 
    mutate(DF.Stock.Price = as.numeric(DF.Stock.Price))
  
  return(DF.historical)
}


  