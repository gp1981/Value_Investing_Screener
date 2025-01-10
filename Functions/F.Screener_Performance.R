# Author: gp1981
# Date: 24 Dec 2021
# Purpose: Calculate screener performance and generate statistic tables for Green, Yellow, Red categories
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: DF_last_FQ_Output_GIG_Top50_Greenblatt - Dataframe containing financial data with Greenblatt ranking and other relevant metrics
# Output: DF_last_FQ_GIG_Performance - A vector containing performance statistics for different screeners


Screener_Performance <- function(DF_last_FQ_Output_GIG_Full_List_Greenblatt) {

  # 00 - Data ranking (US only) --------------------------------------------------
  DF_last_FQ_Output_GIG_Full_List_Greenblatt <- DF_last_FQ_Output_GIG_Full_List_Greenblatt %>% 
    filter(country %in% c("US", "CA"))
  
  DF_last_FQ_Output_GIG_Full_List_Greenblatt <- DF_last_FQ_Output_GIG_Full_List_Greenblatt %>%
    filter(!is.na(Return.On.Capital) & is.finite(Return.On.Capital)) %>%
    arrange(desc(Return.On.Capital)) %>%
    mutate(Rank.Return.On.Capital = dplyr::row_number()) %>%
    select(Ticker, date, quarter, year, Rank.Return.On.Capital, Return.On.Capital, everything())
  
  ## 02.2 - Ranking EY ----
  DF_last_FQ_Output_GIG_Full_List_Greenblatt <- DF_last_FQ_Output_GIG_Full_List_Greenblatt %>% 
    filter(!is.na(Earnings.Yield.Greenblatt) & is.finite(Earnings.Yield.Greenblatt)) %>% 
    arrange(desc(Earnings.Yield.Greenblatt)) %>% 
    mutate(Rank.Earnings.Yield.Greenblatt = dplyr::row_number()) %>% 
    select(Ticker, date, quarter, year, Rank.Earnings.Yield.Greenblatt, Earnings.Yield.Greenblatt, everything())
  
  ## 02.3 - Ranking combined ranking EY and ROC ----
  DF_last_FQ_Output_GIG_Full_List_Greenblatt <- DF_last_FQ_Output_GIG_Full_List_Greenblatt %>% 
    mutate(Rank.Combined.EY_ROC.Greenblatt.Today = Rank.Return.On.Capital + Rank.Earnings.Yield.Greenblatt) %>% 
    select(Ticker, date, quarter, year, Rank.Combined.EY_ROC.Greenblatt.Today, everything())
  
  DF_last_FQ_Output_GIG_Full_List_Greenblatt <- DF_last_FQ_Output_GIG_Full_List_Greenblatt %>% 
    arrange(Rank.Combined.EY_ROC.Greenblatt.Today) %>% 
    mutate(ID_Rank.Combined.EY_ROC.Greenblatt.Today = dplyr::row_number()) %>% 
    select(ID_Rank.Combined.EY_ROC.Greenblatt.Today, everything())
  
  # 01 - Data Preparation ---------------------------------------------------
  
  # Filter data and calculate performance
  IGVI_S_Results_Top50_Greenblatt_Top50 <- DF_last_FQ_Output_GIG_Full_List_Greenblatt %>% 
    filter(!is.na(MF_TopGreenblatt) & ID_Rank.Combined.EY_ROC.Greenblatt.Today <= 50)
  
  IGVI_S_Results_Top30_Greenblatt_Top30 <- DF_last_FQ_Output_GIG_Full_List_Greenblatt %>% 
    filter(MF_TopGreenblatt == "Top30" & ID_Rank.Combined.EY_ROC.Greenblatt.Today <= 30)
  
  # Calculate statistics for the outputs
  IGVI_S_Results_Top50_Greenblatt_Top50_print <-  paste0("No of Top 50 from Magic Formula in common with IGVI Top 50 ", nrow(IGVI_S_Results_Top50_Greenblatt_Top50), "/50 = ", nrow(IGVI_S_Results_Top50_Greenblatt_Top50) / 50 * 100, "%")
  IGVI_S_Results_Top30_Greenblatt_Top30_print <- paste0("No of Top 30 from Magic Formula in common with IGVI Top 30 ", nrow(IGVI_S_Results_Top30_Greenblatt_Top30), "/30 = ", nrow(IGVI_S_Results_Top30_Greenblatt_Top30) / 30 * 100, "%")
  
  # 02 - Prepare output -----------------------------------------------------
  
  # Store the performance statistics in a vector
  DF_last_FQ_GIG_Performance <- c(
    Top50_Greenblatt_Top50_IGVI = IGVI_S_Results_Top50_Greenblatt_Top50_print, 
    Top30_Greenblatt_Top30_IGVI = IGVI_S_Results_Top30_Greenblatt_Top30_print
  )
  
  return(DF_last_FQ_GIG_Performance)
}

# Example usage:
# df_performance <- IGVI_Screener_Performance(df_last_fq_g)
