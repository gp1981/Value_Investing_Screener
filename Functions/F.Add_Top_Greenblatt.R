# Author: gp1981
# Date: 15 Jan 2021
# Purpose: Combine data from the Top_Greenblatt file and the provided DataFrame (DF) using the Ticker column as a common key
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: DF - Dataframe containing financial data
# Output: DF2 - Combined dataframe with the data from both DF and the Top_Greenblatt file


library(readxl)

Add_Top_Greenblatt <- function(df, last_business_date, mktCap_limit_lower_M, mktCap_limit_upper_M, mktCap_step) {
  
  # 01 - Data Loading and Merging -------------------------------------------
  
  df1 <- Import_MF_data(last_business_date, mktCap_limit_lower_M, mktCap_limit_upper_M, mktCap_step)
  
   # Rename columns, excluding a specific column
  prefix = "MF_"
  colnames(df1) <- ifelse(names(df1) == "Ticker", "Ticker", paste0(prefix, names(df1)))
  
  # Export Magic Formula data into excel
  Export_excel_MF_data(df1)
  
  # Merge the Top_Greenblatt data with the provided DataFrame (DF) using the Ticker column as a common key
  df2 <- df %>%
    left_join(df1, by = "Ticker")

  df2 <- df2 %>% 
    mutate(mktCap_M = mktCap / 1e06,
           MF_Threshold_mktCap_M = MF_threshold_mktCap) %>% 
    select(Ticker,date, price, MF_TopGreenblatt, MF_Threshold_mktCap_M, -MF_threshold_mktCap,
          everything())
    
  
  return(df2)
}

# Example usage:
# df_combined <- Add_Top_Greenblatt(df)
