# Author: Gabriele Picheo - 22 Nov. 2024

# INPUT: DF with all fundamentals and maintenance capex
# OUTPUT: 

#   - various multipliers


Multipliers <- function(df){
  
  # 01 - Calculation of multipliers -----------
  
  
  df <- df %>%
    mutate(
      MktCap_EV_IGVI.Op.Assets = marketCap / Enterprise.Value.IGVI.Op.Assets,
      MktCap_EV = marketCap / enterpriseValue,
      
      Debt_EV_IGVI.Op.Assets = totalDebt / Enterprise.Value.IGVI.Op.Assets,
      Debt_EV = totalDebt / enterpriseValue,
      
      CurrentAssets_EV_IGVI.Op.Assets = totalCurrentAssets / Enterprise.Value.IGVI.Op.Assets,
      CurrentAssets_EV = totalCurrentAssets / enterpriseValue,
      
      NonCurrentAssets_EV_IGVI.Op.Assets = totalNonCurrentAssets / Enterprise.Value.IGVI.Op.Assets,
      NonCurrentAssets_EV = totalNonCurrentAssets / enterpriseValue,
      
      TotalAssets_EV_IGVI.Op.Assets = totalAssets / Enterprise.Value.IGVI.Op.Assets,
      totalAssets_EV = totalAssets / enterpriseValue,
      
      ExcessCash_EV_IGVI.Op.Assets = Excess.Cash / Enterprise.Value.IGVI.Op.Assets,
      ExcessCash_EV = Excess.Cash / enterpriseValue,
      
      NetWorkingCapital_EV_IGVI.Op.Assets = Net.Working.Capital / Enterprise.Value.IGVI.Op.Assets,
      NetWorkingCapital_EV = Net.Working.Capital / enterpriseValue
    ) 

  
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
