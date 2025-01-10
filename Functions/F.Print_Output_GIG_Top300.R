# Author: gp1981
# Date: 24 Dec 2022
# Purpose: Generate statistic tables for Green, Yellow, Red categories and screener performance from the provided DataFrame (DF_last_FQ_G)
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: DF_last_FQ_G - Dataframe containing financial data with Greenblatt ranking and other relevant metrics
# Output: DF_last_FQ_GIG_Output_Top300 - Dataframe with filtered and sorted data based on the top 300 Greenblatt ranking stocks

Print_Output_GIG_Top300 <- function(DF_last_FQ_G) {
  
  # 01 - Data Preparation ---------------------------------------------------
  
  # Create a new dataframe for the output
  DF_last_FQ_GIG_Output_Top300 <- DF_last_FQ_G
  
  # Filter out rows with ID_Rank.Combined.EY_ROC.Greenblatt.Today less than or equal to 300 and assign output categories based on specific conditions
  DF_last_FQ_GIG_Output_Top300 <- DF_last_FQ_GIG_Output_Top300 %>% 
    filter(ID_Rank.Combined.EY_ROC.Greenblatt.Today <= 300) %>% 
    mutate(Output = dplyr::case_when(
      Current.ratio >= 2 & 
        Acid.ratio_Cash >= 1 & 
        Capital.Structure.E_over_D >= 0.6 & 
        MoS_Percentage >= 0 ~ "GREEN",
      Current.ratio >= 1 & 
        Acid.ratio_Cash >= 0.3 & 
        Capital.Structure.E_over_D >= 0 &
        MoS_Percentage >= -0.5 ~ "YELLOW",
      TRUE ~ "RED"
    )) %>% 
    select(Output, Outlier.Revenue.ZScore, Top_Greenblatt, Ticker, companyName, country, price, mktCap, Acid.ratio_Cash,
           Acid.ratio, Current.ratio, Capital.Structure.E_over_D,
           Net.Current.Asset.over.Debt, Net.Book.Value.per.Share, Net.Current.Asset.Value.per.Share,
           Net.Cash.Asset.Value.per.Share, Liq.Value.per.Share, Price_over_Net.Book.Value, MoS_Percentage, everything()) %>% 
    arrange(desc(MoS_Percentage), ID_Rank.Combined.EY_ROC.Greenblatt.Today)
  
  # 02 - Prepare output -----------------------------------------------------
  
  return(DF_last_FQ_GIG_Output_Top300)
}

# Example usage:
# df_output <- IGVI_Print_Output_GIG_Top300(df_last_fq_g)
