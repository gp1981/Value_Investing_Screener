# Author: gp1981
# Purpose: Calculate DataFrame of calculation and stock ranking (Graham No.) for the last FQ based on the available FY data from FinancialModellingPrep.com
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: DF_last_FQ - Dataframe containing the last FQ calculations and rankings
# Output: DF - Dataframe with calculated Graham values (Book, Current, Cash), liquidation values, margin of safety, and other ratios

Graham_MoS_Rank_last_FQ <- function(DF_last_FQ) {
  
  # 01 - Calculate Graham values (Book, Current, Cash) ----------------------
  
  DF <- DF_last_FQ %>%
    mutate(Outstanding.Shares = as.numeric(mktCap) / as.numeric(price)) %>%
    mutate(Net.Book.Value.per.Share =
             (as.numeric(totalAssets) - as.numeric(goodwillAndIntangibleAssets) -
                as.numeric(totalLiabilities) - as.numeric(minorityInterest)) / as.numeric(Outstanding.Shares)) %>%
    mutate(Net.Current.Asset.Value.per.Share =
             (as.numeric(totalCurrentAssets) - as.numeric(totalLiabilities)) / as.numeric(Outstanding.Shares)) %>%
    mutate(Net.Cash.Asset.Value.per.Share =
             (as.numeric(cashAndShortTermInvestments) - as.numeric(totalLiabilities)) / as.numeric(Outstanding.Shares)) %>%
    select(Ticker, date, price,
           Net.Book.Value.per.Share, Net.Current.Asset.Value.per.Share, Net.Cash.Asset.Value.per.Share, everything())
  
  # 02 - Calculate Graham liquidation values ----------------------
  
  DF <- DF %>%
    mutate(Acid.ratio_Cash = as.numeric(cashAndShortTermInvestments) / as.numeric(totalCurrentLiabilities)) %>%
    mutate(Acid.ratio = (as.numeric(cashAndShortTermInvestments) + as.numeric(netReceivables)) / as.numeric(totalCurrentLiabilities)) %>%
    mutate(Current.ratio = as.numeric(totalCurrentAssets) / as.numeric(totalCurrentLiabilities)) %>%
    mutate(Net.Current.Asset.over.Debt = (as.numeric(totalCurrentAssets) - as.numeric(totalCurrentLiabilities)) / as.numeric(totalDebt)) %>%
    mutate(Capital.Structure.E_over_D = as.numeric(totalStockholdersEquity) / as.numeric(totalDebt)) %>%
    mutate(Capital.Structure.D_over_A = as.numeric(totalDebt) / as.numeric(totalAssets)) %>%
    mutate(Liq.Receivable_75pc = as.numeric(netReceivables) * 0.75) %>%
    mutate(Liq.Inventory_50pc = as.numeric(inventory) * 0.5) %>%
    mutate(Liq.PPE_15pc = as.numeric(propertyPlantEquipmentNet) * 0.15) %>%
    mutate(Liq.Other.Asset_15pc = as.numeric(otherAssets) * 0.15)
  
  DF <- DF %>%
    mutate(Liq.Value.per.Share = ((as.numeric(cashAndShortTermInvestments) +
                                     Liq.Receivable_75pc +
                                     Liq.Inventory_50pc +
                                     Liq.PPE_15pc +
                                     Liq.Other.Asset_15pc - as.numeric(totalLiabilities) -
                                     as.numeric(minorityInterest)
    ) / as.numeric(Outstanding.Shares)))
  
  # 03 - Calculate Graham Margin of Safety ----------------------
  
  DF <- DF %>%
    mutate(MoS = Liq.Value.per.Share - as.numeric(price)) %>%
    mutate(MoS_Percentage = Liq.Value.per.Share / as.numeric(price) - 1)
  
  # 04 - Calculate other ratios ----------------------
  
  DF <- DF %>%
    mutate(Price_over_Net.Book.Value = as.numeric(price) / Net.Book.Value.per.Share)
  
  # 05 - Prepare output ----------------------
  DF <- DF %>%
    select(Ticker, date, price,
           MoS, MoS_Percentage, Liq.Value.per.Share, 
           Capital.Structure.E_over_D,Capital.Structure.D_over_A,
           Net.Current.Asset.over.Debt, 
           Acid.ratio_Cash,
           Acid.ratio, Current.ratio, 
           everything())
  
  return(DF)
}

# Example usage:
# df_last_fq_graham <- Graham_MoS_Rank_last_FQ(df_last_fq)
