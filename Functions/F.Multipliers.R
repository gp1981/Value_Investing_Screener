# Author: Gabriele Picheo - 22 Nov. 2024

# INPUT: DF with all fundamentals and maintenance capex
# OUTPUT: 

#   - various multipliers


Multipliers <- function(df){
  
  # 01 - Calculation of multipliers -----------
  
  
  df <- df %>%
    mutate(
      MarketCap_over_EV_IGVI.Op.Assets = marketCap_EV / Enterprise.Value.IGVI.Op.Assets,
      MarketCap_over_EV = marketCap_EV / enterpriseValue_EV,
      
      Debt_over_EV_IGVI.Op.Assets = totalDebt / Enterprise.Value.IGVI.Op.Assets,
      Debt_over_EV = totalDebt / enterpriseValue_EV,
      
      CurrentAssets_over_EV_IGVI.Op.Assets = totalCurrentAssets / Enterprise.Value.IGVI.Op.Assets,
      CurrentAssets_over_EV = totalCurrentAssets / enterpriseValue_EV,
      
      NonCurrentAssets_over_EV_IGVI.Op.Assets = totalNonCurrentAssets / Enterprise.Value.IGVI.Op.Assets,
      NonCurrentAssets_over_EV = totalNonCurrentAssets / enterpriseValue_EV,
      
      TotalAssets_over_EV_IGVI.Op.Assets = totalAssets / Enterprise.Value.IGVI.Op.Assets,
      totalAssets_over_EV = totalAssets / enterpriseValue_EV,
      
      ExcessCash_over_EV_IGVI.Op.Assets = Excess.Cash / Enterprise.Value.IGVI.Op.Assets,
      ExcessCash_over_EV = Excess.Cash / enterpriseValue_EV,
      
      NetWorkingCapital_over_EV_IGVI.Op.Assets = Net.Working.Capital / Enterprise.Value.IGVI.Op.Assets,
      NetWorkingCapital_over_EV = Net.Working.Capital / enterpriseValue_EV
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
      CAGR.full.Equity_noDebt,
      no.quarters.FCF_negative_ratio,
      everything()
    )
  return(df)
}
