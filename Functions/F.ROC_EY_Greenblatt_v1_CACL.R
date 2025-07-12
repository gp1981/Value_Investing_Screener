# Author: gp1981
# Purpose: Perform the calculation of the MF parameters ROC and EY
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

library(zoo)

ROC_EY_Greenblatt_v1_CACL <- function(DF) {
  
  # 01 - Income Statement data of the last fiscal year ----------------------
  
  DF <- DF %>%
    mutate(last.business.date = as.Date(last_business_date, format = "%Y-%m-%d")) %>% 
    mutate(quarter = floor_date(date, "quarter")) %>% 
    mutate(year = floor_date(date, "year"))
  
  ## 02 - Calculation of Excess of Cash, NWC, TCE ----
  DF <- Excess_Cash(DF)
  
  ## 02.2 - Calculation of 4FQ rolling sum of Op. Income and z-score of revenue over the last 12 FQ ----
  DF <- DF %>% 
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(Revenue.4FQ = rollapply(revenue,
                                   width = 4, FUN = sum, align = "left", fill = NA),
           EBIT.4FQ = rollapply(operatingIncome,
                                width = 4, FUN = sum, align = "left", fill = NA),
           FCF.4FQ = rollapply(freeCashFlow,
                               width = 4, FUN = sum, align = "left", fill = NA),
           Op_CashFlow.4FQ = rollapply(netCashProvidedByOperatingActivities,
                                       width = 4, FUN = sum, align = "left", fill = NA),
           Fin.CashFlow.4FQ = rollapply(netCashProvidedByInvestingActivities,
                                        width = 4, FUN = sum, align = "left", fill = NA),
           Inv_CashFlow.4Q = rollapply(netCashProvidedByFinancingActivities,
                                       width = 4, FUN = sum, align = "left", fill = NA),
           Capex.4FQ = rollapply(capitalExpenditure,
                                 width = 4, FUN = sum, align = "left", fill = NA),
           IQR.Revenue = rollapply(revenue, 
                                   width = 8, FUN = IQR, align = "left", na.rm = TRUE, fill = NA),
           Mean.Revenue = rollapply(revenue, 
                                    width = 8, FUN = mean, align = "left", na.rm = TRUE, fill = NA),
           Revenue.ZScore = (revenue - Mean.Revenue) / sd(revenue)
    ) %>% 
    mutate(Outlier.Revenue.ZScore = dplyr::case_when(
      Revenue.ZScore > 2 ~ "High",
      Revenue.ZScore < -2 ~ "Low",
      TRUE ~ "OK"
    )) %>% 
    ungroup()
  
  # 03 - Calculate EY and ROC -----------------------------------------
  
  # Replace the first raw value
  DF <- DF %>% 
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
      mutate(
      marketCap_LocalFX = ifelse(row_number() == 1, marketCap_USD_Profile/FX_rates, marketCapitalization_EV_LocalFX_EV)
      )

  DF <- DF %>% 
    mutate(
      Tangible_Equity_book = totalAssets - totalLiabilities - 
        goodwillAndIntangibleAssets,
      
      Equity_Net_Premium = marketCap_LocalFX - Tangible_Equity_book,
      
      Equity_Net_premiumToFCF= Equity_Net_Premium / FCF.4FQ,
      
      Net.Working.Capital = totalCurrentAssets - Excess.Cash
      - (totalCurrentLiabilities - shortTermDebt),
      
      Tangible.Capital.Employed = (totalAssets - otherCurrentAssets
                                   - goodwillAndIntangibleAssets -
                                     otherNonCurrentAssets - 
                                     otherAssets - Excess.Cash) -
        (totalCurrentLiabilities - 
           capitalLeaseObligationsCurrent - 
           otherCurrentLiabilities -
           shortTermDebt),
      
      Return.On.Capital = EBIT.4FQ / Tangible.Capital.Employed,
      
      Net.Interest.Bearing.Debt = totalDebt + capitalLeaseObligations,
      
      Enterprise.Value.Greenblatt = marketCap_LocalFX  + Net.Interest.Bearing.Debt 
      + minorityInterest + preferredStock,
      
      Enterprise.Value.IGVI.Op.Assets = marketCap_LocalFX + totalLiabilities - goodwillAndIntangibleAssets -
        Excess.Cash,
      
      Earnings.Yield.Greenblatt = EBIT.4FQ / Enterprise.Value.Greenblatt,
      
      Earnings.Yield.Op.Assets = EBIT.4FQ / Enterprise.Value.IGVI.Op.Assets
      
    )
  
  # 04 - Correct misspelled variables and data ---------------------------
  
  DF <- DF %>% 
    mutate(sector = gsub("Energy ", "Energy", sector)) 
  
  
  # 05 - Prepare output -----------------------------------------------------
  
  DF <- select(DF,Ticker,date,
               Earnings.Yield.Greenblatt,
               Enterprise.Value.IGVI.Op.Assets,
               Return.On.Capital,
               Equity_Net_premiumToFCF,
               Equity_Net_Premium,
               Tangible_Equity_book,
               EBIT.4FQ,
               Net.Interest.Bearing.Debt,
               Tangible.Capital.Employed,
               everything())
  return(DF)
}
