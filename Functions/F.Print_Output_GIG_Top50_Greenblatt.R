# Author: gp1981
# Date: 24 Dec 2021
# Purpose: Generate statistic tables for Green, Yellow, Red categories and screener performance from the provided DataFrame (DF_last_FQ_G)
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: DF_last_FQ_G - Dataframe containing financial data with Greenblatt ranking and other relevant metrics
# Output: DF_last_FQ_GIG_Output_Top50_Greenblatt - Dataframe with filtered and sorted data based on Green, Yellow, Red categories and Greenblatt ranking


Print_Output_GIG_Top50_Greenblatt <- function(df) {
  
  # 01 - Data Preparation ---------------------------------------------------
  
  # Create a new dataframe for the output
  DF_last_FQ_GIG_Output_Top50_Greenblatt <- df
  
  # Assign output categories based on specific conditions
  DF_last_FQ_GIG_Output_Top50_Greenblatt <- DF_last_FQ_GIG_Output_Top50_Greenblatt %>% 
    mutate(Output = dplyr::case_when(
      quickRatioTTM >= 2 & 
        cashRatioTTM >= 1 & 
        debtToEquityRatioTTM <= 0.33 & 
        debtToAssetsRatioTTM <= 0.2 ~ "GREEN",
      
      quickRatioTTM >= 1 & 
        cashRatioTTM >= 0.3 & 
        debtToEquityRatioTTM <= 1 &
        debtToAssetsRatioTTM <= 0.35 ~ "YELLOW",
        
      TRUE ~ "RED"
    )) %>% 
    
    mutate(Enterprise.Value.IGVI.Op.Assets_M = Enterprise.Value.IGVI.Op.Assets / 1000000) %>%
    relocate(Enterprise.Value.IGVI.Op.Assets, .after = last_col()) %>% 
  
    select(Output, Outlier.Revenue.ZScore, MF_TopGreenblatt, MF_Threshold_marketCap_M, Ticker, companyName, 
           country, price, marketCap_M,ID_Rank.Combined.EY_ROC.Greenblatt,Enterprise.Value.IGVI.Op.Assets_M,
           Earnings.Yield.Greenblatt, Return.On.Capital, Earning.Power.per.Share.TTM,
           Owner.Earnings.Buffet.per.Share.TTM, Owner.Earnings.Buffet.IGVI.per.Share.TTM, 
           Owner.Earnings.IGVI.per.Share.TTM, priceToEarningsRatioTTM, priceToBookRatioTTM, 
           Price_over_Net.Book.Value, CAGR.full.Equity, CAGR.full.Equity_noDebt, no.quarters.FCF_negative_ratio, 
           Equity_Net_premiumToFCF, debtToEquityRatioTTM, Capital.Structure.E_over_D, 
           debtToAssetsRatioTTM, Capital.Structure.D_over_A, longTermDebtToCapitalRatioTTM, Acid.ratio_Cash, 
           quickRatioTTM, currentRatioTTM, Net.Book.Value.per.Share, Net.Current.Asset.Value.per.Share,
           Net.Cash.Asset.Value.per.Share, Liq.Value.per.Share,  MoS_Ratio, everything()) %>% 
    arrange(ID_Rank.Combined.EY_ROC.Greenblatt)
  
  # 02 - Prepare output -----------------------------------------------------
  
  return(DF_last_FQ_GIG_Output_Top50_Greenblatt)
}

# Example usage:
# df_output <- IGVI_Print_Output_GIG_Top50_Greenblatt(df_last_fq_g)
