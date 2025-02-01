# Author: gp1981

# Purpose: Calculate DataFrame of calculation and stock ranking (Greenblatt EY & ROC) for the last FQ based on the trailing 12-month data
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: ROC_EY - Dataframe containing Return on Capital and Earnings Yield information calculated based on the trailing 12-month data
#        mktCap_limit_lower_M - Market capitalization limit for filtering
# Output: DF_last_FQ - Dataframe with calculated rankings for the last FQ

DataFrame_last_FQ <- function(ROC_EY, mktCap_limit_lower_M,country) {
  
 # 01 - Filtering by mktcap and sectors ------------------------------------
  
  ## 01.1 - Define the number of last useful Fiscal Quarters ----
  
  # Filter out rows with a date greater than today
  ROC_EY <- ROC_EY %>% 
    filter(!(date > today()))
  
  # Calculate the count of quarters and drop NA values
  Last.Quarters <- ROC_EY %>%  
    group_by(quarter) %>% 
    dplyr::summarize(n())
  
  Last.Quarters <- Last.Quarters %>% 
    drop_na(quarter)
  
  # Filter the last 4 quarters
  Last.Quarters <- Last.Quarters %>% 
    filter(row_number() >= (n() - 3)) %>% 
    arrange(desc(quarter))
  
  # Before filtering ROC_EY, check if at least 4 quarters exist:
  if (nrow(Last.Quarters) >= 4) {
    DF_last_FQ <- ROC_EY %>%
      group_by(Ticker) %>%
      arrange(desc(date)) %>% 
      slice(1) %>% 
      filter(
        quarter > as.Date(Last.Quarters$quarter[4]) & quarter <= as.Date(Last.Quarters$quarter[1])
      ) %>%
      select(Ticker, year, quarter, date, companyName, industry, sector, 
             Return.On.Capital, Earnings.Yield.Greenblatt, everything())
  } else {
    warning("Less than 4 quarters available. Adjusting filter criteria.")
    
    DF_last_FQ <- ROC_EY %>%
      group_by(Ticker) %>%
      arrange(desc(date)) %>% 
      slice(1) %>% 
      filter(
        quarter > min(Last.Quarters$quarter) & quarter <= max(Last.Quarters$quarter)
      ) %>%
      select(Ticker, year, quarter, date, companyName, industry, sector, 
             Return.On.Capital, Earnings.Yield.Greenblatt, everything())
  }  

  # 02 - Greenblatt Ranking --------------------------------------------------
  
  # Create auxiliary ranking column for grouping purpose
  DF_last_FQ <- DF_last_FQ %>%
    mutate(aux.rank = "A")
  
  ## 02.1 - Ranking ROC ----
  if ("plyr" %in% search()) {
    # Detach plyr if it is loaded
    detach("package:plyr", unload = TRUE)
  }
  
  DF_last_FQ <- DF_last_FQ %>%
    filter(!is.na(Return.On.Capital) & is.finite(Return.On.Capital)) %>%
    group_by(aux.rank) %>%
    arrange(desc(Return.On.Capital)) %>%
    mutate(Rank.Return.On.Capital = dplyr::row_number()) %>%
    select(Ticker, date, quarter, year, Rank.Return.On.Capital, Return.On.Capital, everything())
  
  ## 02.2 - Ranking EY ----
  DF_last_FQ <- DF_last_FQ %>% 
    filter(!is.na(Earnings.Yield.Greenblatt) & is.finite(Earnings.Yield.Greenblatt)) %>% 
    group_by(aux.rank) %>%
    arrange(desc(Earnings.Yield.Greenblatt)) %>% 
    mutate(Rank.Earnings.Yield.Greenblatt = dplyr::row_number()) %>% 
    select(Ticker, date, quarter, year, Rank.Earnings.Yield.Greenblatt, Earnings.Yield.Greenblatt, everything())
  
  ## 02.3 - Ranking combined ranking EY and ROC ----
  DF_last_FQ <- DF_last_FQ %>% 
    mutate(Rank.Combined.EY_ROC.Greenblatt = Rank.Return.On.Capital + Rank.Earnings.Yield.Greenblatt) %>% 
    select(Ticker, date, quarter, year, Rank.Combined.EY_ROC.Greenblatt, everything())
  
  DF_last_FQ <- DF_last_FQ %>% 
    group_by(aux.rank) %>%
    arrange(Rank.Combined.EY_ROC.Greenblatt) %>% 
    mutate(ID_Rank.Combined.EY_ROC.Greenblatt = dplyr::row_number()) %>% 
    select(ID_Rank.Combined.EY_ROC.Greenblatt, everything())
  
  # 03 - Ranking by ROA PE -----------------------------------------------------
  
  ## 03.1 - Ranking ROA ----
  DF_last_FQ <- DF_last_FQ %>% 
    mutate(Return.On.Total.Assets = as.numeric(EBIT.4FQ) / as.numeric(totalAssets)) %>% 
    group_by(aux.rank) %>%
    arrange(desc(Return.On.Total.Assets)) %>% 
    mutate(Rank.Return.On.Total.Assets = dplyr::row_number())
  
  ## 03.2 - Ranking by PE ----
  DF_last_FQ <- DF_last_FQ %>% 
    group_by(aux.rank) %>%
    arrange(peRatioTTM) %>% 
    mutate(Rank.PE.ratio = dplyr::row_number())
  
  ## 03.3 - Ranking combined ranking ROA and PE ----
  DF_last_FQ <- DF_last_FQ %>% 
    mutate(Rank.Combined.ROTA_PE = Rank.Return.On.Total.Assets + Rank.PE.ratio) %>% 
    group_by(aux.rank) %>%
    arrange(Rank.Combined.ROTA_PE) %>% 
    mutate(ID_Rank.Combined.ROTA_PE = dplyr::row_number())
  
  DF_last_FQ <- DF_last_FQ %>% 
    select(Ticker, date, quarter, 
           ID_Rank.Combined.EY_ROC.Greenblatt, 
           ID_Rank.Combined.ROTA_PE, -aux.rank, 
           everything())
  
  # Return the final dataframe
  return(DF_last_FQ)
}

# Example usage:
# df_last_fq <- DataFrame_last_US_FQ(roc_ey_data, market_cap_limit)
