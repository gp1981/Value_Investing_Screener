# Author: gp1981
# Purpose: Calculate DataFrame of calculation and stock ranking (Graham No.) for the last FQ based on the available FY data from FinancialModellingPrep.com
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: DF_last_FQ - Dataframe containing the last FQ calculations and rankings
# Output: DF - Dataframe with calculated Graham values (Book, Current, Cash), liquidation values, margin of safety, and other ratios

Ratios_Graham_MoS <- function(DF) {
  
  # 01 - Calculate Graham values (Book, Current, Cash) ----------------------
  
  DF <- DF %>%
    mutate(
      Net.Book.Value.per.Share =
        ((totalAssets) - (goodwillAndIntangibleAssets) -
           (totalLiabilities) - (minorityInterest)) / (outstandingShares),
      
      Net.Current.Asset.Value.per.Share =
        ((totalCurrentAssets) - (totalLiabilities)) / (outstandingShares),
      
      Net.Cash.Asset.Value.per.Share =
        ((cashAndShortTermInvestments) - (totalLiabilities)) / (outstandingShares)
    ) %>% 
    select(Ticker, date, price,
           Net.Book.Value.per.Share, Net.Current.Asset.Value.per.Share, Net.Cash.Asset.Value.per.Share, everything())
  
  # 02 - Calculate Graham liquidation values ----------------------
  
  DF <- DF %>%
    mutate(
      Acid.ratio_Cash = (cashAndShortTermInvestments) / (totalCurrentLiabilities),
      
      Acid.ratio = ((cashAndShortTermInvestments) + (netReceivables)) / (totalCurrentLiabilities),
      
      Current.ratio = (totalCurrentAssets) / (totalCurrentLiabilities),
      
      Net.Current.Asset.over.Debt = ((totalCurrentAssets) - (totalCurrentLiabilities)) / (totalDebt),
      
      Capital.Structure.E_over_D = (totalStockholdersEquity) / (totalDebt),
      
      Capital.Structure.D_over_A = (totalDebt) / (totalAssets),
      
      Liq.Receivable_75pc = (netReceivables) * 0.75,
      
      Liq.Inventory_50pc = (inventory) * 0.5,
      
      Liq.PPE_15pc = (propertyPlantEquipmentNet) * 0.15,
      
      Liq.Other.Asset_15pc = (otherAssets) * 0.15
    )
  
  DF <- DF %>%
    mutate(Liq.Value.per.Share = (((cashAndShortTermInvestments) +
                                     Liq.Receivable_75pc +
                                     Liq.Inventory_50pc +
                                     Liq.PPE_15pc +
                                     Liq.Other.Asset_15pc - (totalLiabilities) -
                                     (minorityInterest)
    ) / (outstandingShares)))
  
  # 03 - Calculate Graham Margin of Safety and ratios ----------------------
  
  DF <- DF %>%
    mutate(
      MoS = Liq.Value.per.Share * outstandingShares - marketCap,
      MoS_Ratio = (Liq.Value.per.Share  * outstandingShares) / marketCap - 1,
      Price_over_Net.Book.Value = marketCap / (Net.Book.Value.per.Share * outstandingShares)
    )
  
  
  # 04 - Prepare output ----------------------
  DF <- DF %>%
    select(Ticker, date, price,
           MoS, MoS_Ratio, Liq.Value.per.Share, 
           Capital.Structure.E_over_D,Capital.Structure.D_over_A,
           Net.Current.Asset.over.Debt, 
           Acid.ratio_Cash,
           Acid.ratio, Current.ratio, 
           everything())
  
  return(DF)
}
