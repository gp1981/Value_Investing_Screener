# Author: gp1981

# Purpose: Calculate DataFrame of calculation and stock ranking (Greenblatt EY & ROC) over all quarters
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: ROC_EY - Dataframe containing Return on Capital and Earnings Yield information calculated based on the trailing 12-month data
#        mktCap_limit_lower_M - Market capitalization limit for filtering
# Output: DF_all_FQ - Dataframe with calculated rankings for the last FQ

DataFrame_all_FQ <- function(ROC_EY, mktCap_limit_lower_M,country) {
  
  # 01 - Filtering by mktcap and sectors ------------------------------------
  
  ## 01.1 - Define the number of last useful Fiscal Quarters ----
  
  # Filter out rows with a date greater than today
  ROC_EY <- ROC_EY %>% 
    filter(!(date > today()))
  
  # Calculate the count of quarters and drop NA values
  Last.Quarters <- ROC_EY %>%  
    group_by(quarter) %>% 
    summarise(count = n(), .groups = "drop") %>%  # Renaming count for clarity
    arrange(desc(quarter)) %>%   # Ensure most recent quarter is first
    drop_na(quarter)
  
  Last.Quarters <- Last.Quarters %>% 
    drop_na(quarter)
  
  # 02 - Greenblatt Ranking --------------------------------------------------
  
  ## 02.1 - Ranking ROC ----
  if ("plyr" %in% search()) {
    # Detach plyr if it is loaded
    detach("package:plyr", unload = TRUE)
  }
  
  ## 02.1.1 - Ranking Return On Capital and Calculating Average ----
  DF_all_FQ <- ROC_EY %>%
    filter(!is.na(Return.On.Capital) & is.finite(Return.On.Capital)) %>%
    group_by(Ticker) %>%  # Group by Ticker for correct trailing mean calculation
    arrange(quarter) %>%  # Ensure quarters are ordered from oldest to most recent
    mutate(
      Rolling_Avg_Return.On.Capital = zoo::rollapply(
        Return.On.Capital, 
        width = seq_along(Return.On.Capital),  # Use seq_along to make the window dynamic per row
        FUN = mean, 
        na.rm = TRUE, 
        align = "right"  # Trailing average up to the current quarter
      )
    ) %>%
    arrange(Ticker, desc(date)) %>% 
    select(Ticker, date, quarter, year,  Rolling_Avg_Return.On.Capital, Return.On.Capital, everything()) %>%
    ungroup()
  
  ## 03.1 - Ranking Rolling avg. ROIC ----
  DF_all_FQ <- DF_all_FQ %>% 
    group_by(quarter) %>%
    arrange(desc(Rolling_Avg_Return.On.Capital)) %>% 
    mutate(Rank.Rolling_Avg_Return.On.Capital = dplyr::row_number()) %>% 
    select(Ticker, date, quarter, year, Rank.Rolling_Avg_Return.On.Capital, Rolling_Avg_Return.On.Capital, everything())
  
  ## 03.2 - Ranking ROIC ----
  DF_all_FQ <- DF_all_FQ %>% 
    group_by(quarter) %>%
    arrange(desc(Return.On.Capital)) %>% 
    mutate(Rank.Return.On.Capital = dplyr::row_number()) %>% 
    select(Ticker, date, quarter, year, Rank.Return.On.Capital, Return.On.Capital, everything())
  
  ## 03.3 - Ranking EY ----
  DF_all_FQ <- DF_all_FQ %>% 
    filter(!is.na(Earnings.Yield.Greenblatt) & is.finite(Earnings.Yield.Greenblatt)) %>% 
    group_by(quarter) %>%
    arrange(desc(Earnings.Yield.Greenblatt)) %>% 
    mutate(Rank.Earnings.Yield.Greenblatt = dplyr::row_number()) %>% 
    select(Ticker, date, quarter, year, Rank.Earnings.Yield.Greenblatt, Earnings.Yield.Greenblatt, everything())
  
  ## 03.4 - Ranking CAGR_Full_Equity ----
  DF_all_FQ <- DF_all_FQ %>% 
    filter(!is.na(CAGR.full.Equity) & is.finite(CAGR.full.Equity)) %>% 
    group_by(quarter) %>%
    arrange(desc(CAGR.full.Equity)) %>% 
    mutate(Rank.CAGR.full.Equity = dplyr::row_number()) %>% 
    select(Ticker, date, quarter, year, Rank.CAGR.full.Equity, CAGR.full.Equity, everything())
  
  ## 03.5 - Ranking combined ranking EY and ROC ----
  DF_all_FQ <- DF_all_FQ %>% 
    mutate(Rank.Combined.EY_ROC.Greenblatt = Rank.Return.On.Capital + Rank.Earnings.Yield.Greenblatt) %>% 
    select(Ticker, date, quarter, year, Rank.Combined.EY_ROC.Greenblatt, everything())
  
  ## 03.6 - Ranking combined ranking EY and Rolling avg. ROC and CAGR.Full.Equity----
  DF_all_FQ <- DF_all_FQ %>% 
    mutate(Rank.Combined.EY_Roll_ROC_Full.Equity = Rank.Rolling_Avg_Return.On.Capital + 
             Rank.Earnings.Yield.Greenblatt + Rank.CAGR.full.Equity) %>% 
    select(Ticker, date, quarter, year, Rank.Combined.EY_Roll_ROC_Full.Equity, everything())
  
  ## 04 - Total Ranking -----
  DF_all_FQ <- DF_all_FQ %>% 
    group_by(quarter) %>%
    arrange(Rank.Combined.EY_ROC.Greenblatt) %>% 
    mutate(ID_Rank.Combined.EY_ROC.Greenblatt = dplyr::row_number()) %>% 
    select(ID_Rank.Combined.EY_ROC.Greenblatt, everything())
  
  DF_all_FQ <- DF_all_FQ %>% 
    group_by(quarter) %>%
    arrange(Rank.Combined.EY_Roll_ROC_Full.Equity) %>% 
    mutate(ID_Rank.Combined.EY_Roll_ROC_Full.Equity = dplyr::row_number()) %>% 
    select(ID_Rank.Combined.EY_Roll_ROC_Full.Equity, everything())
  
  # 05 - Ranking by ROA PE -----------------------------------------------------
  
  ## 05.1 - Ranking ROA ----
  DF_all_FQ <- DF_all_FQ %>% 
    mutate(Return.On.Total.Assets = as.numeric(EBIT.4FQ) / as.numeric(totalAssets)) %>% 
    group_by(quarter) %>%
    arrange(desc(Return.On.Total.Assets)) %>% 
    mutate(Rank.Return.On.Total.Assets = dplyr::row_number())
  
  ## 05.2 - Ranking by PE ----
  DF_all_FQ <- DF_all_FQ %>% 
    group_by(quarter) %>%
    arrange(peRatioTTM) %>% 
    mutate(Rank.PE.ratio = dplyr::row_number())
  
  ## 05.3 - Ranking combined ranking ROA and PE ----
  DF_all_FQ <- DF_all_FQ %>% 
    mutate(Rank.Combined.ROTA_PE = Rank.Return.On.Total.Assets + Rank.PE.ratio) %>% 
    group_by(quarter) %>%
    arrange(Rank.Combined.ROTA_PE) %>% 
    mutate(ID_Rank.Combined.ROTA_PE = dplyr::row_number())
  
  DF_all_FQ <- DF_all_FQ %>% 
    select(Ticker, date, quarter, 
           ID_Rank.Combined.EY_ROC.Greenblatt, 
           ID_Rank.Combined.ROTA_PE,
           everything())
  
  # Return the final dataframe
  return(DF_all_FQ)
}

# Example usage:
# DF_all_FQ <- DataFrame_last_US_FQ(roc_ey_data, market_cap_limit)
