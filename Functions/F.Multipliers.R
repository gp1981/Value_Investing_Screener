# Author: Gabriele Picheo - 22 Nov. 2024

# INPUT: DF with all fundamentals and maintenance capex
# OUTPUT: 

#   - various multipliers


Multipliers <- function(df){
  
  # 01 - Calculation of multipliers -----------
  
  
  df <- df %>%
    mutate(
      MarketCap_over_EV_IGVI.Op.Assets = marketCap_LocalFX / Enterprise.Value.IGVI.Op.Assets,
      MarketCap_over_EV = marketCap_LocalFX / enterpriseValue_EV,
      
      Debt_over_EV_IGVI.Op.Assets = totalDebt / Enterprise.Value.IGVI.Op.Assets,
      Debt_over_EV = totalDebt / enterpriseValueTTM_LocalFX_KM_TTM,
      
      CurrentAssets_over_EV_IGVI.Op.Assets = totalCurrentAssets / Enterprise.Value.IGVI.Op.Assets,
      CurrentAssets_over_EV = totalCurrentAssets / enterpriseValueTTM_LocalFX_KM_TTM,
      
      NonCurrentAssets_over_EV_IGVI.Op.Assets = totalNonCurrentAssets / Enterprise.Value.IGVI.Op.Assets,
      NonCurrentAssets_over_EV = totalNonCurrentAssets / enterpriseValueTTM_LocalFX_KM_TTM,
      
      TotalAssets_over_EV_IGVI.Op.Assets = totalAssets / Enterprise.Value.IGVI.Op.Assets,
      totalAssets_over_EV = totalAssets / enterpriseValueTTM_LocalFX_KM_TTM,
      
      ExcessCash_over_EV_IGVI.Op.Assets = Excess.Cash / Enterprise.Value.IGVI.Op.Assets,
      ExcessCash_over_EV = Excess.Cash / enterpriseValueTTM_LocalFX_KM_TTM,
      
      NetWorkingCapital_over_EV_IGVI.Op.Assets = Net.Working.Capital / Enterprise.Value.IGVI.Op.Assets,
      NetWorkingCapital_over_EV = Net.Working.Capital / enterpriseValueTTM_LocalFX_KM_TTM
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
