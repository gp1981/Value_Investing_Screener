# Author: gp1981
# Purpose: Combine the input dataframe with historical S&P500 constituents
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: df - Input dataframe
#        DF_SP500_all_FQ - Dataframe containing historical S&P500 constituents
# Output: Combined dataframe with S&P500 information


Combine_SP500 <- function(df, DF_SP500_all_FQ) {
  
  # 01 - Combine with historical S&P500 constituents ---------------------------
  
  # Add month, quarter, and year columns to the input dataframe
  df <- df %>% 
    mutate(month = floor_date(date, "month")) %>% 
    mutate(quarter = floor_date(date, "quarter")) %>% 
    mutate(year = floor_date(date, "year"))
  
  # Add month, quarter, and year columns to the S&P500 dataframe and select relevant columns
  df2_SP500 <- DF_SP500_all_FQ %>% 
    mutate(month = floor_date(date, "month")) %>% 
    mutate(quarter = floor_date(date, "quarter")) %>% 
    mutate(year = floor_date(date, "year")) %>% 
    mutate(date_original = date) %>% 
    select(date_original, month, year, Ticker) %>% 
    mutate(SP500 = "SP500") %>% 
    distinct(month, year, Ticker, .keep_all = TRUE)
  
  # 02 - Prepare output ---------------------------------------------------------
  
  # Perform a left join between the input dataframe and the S&P500 dataframe based on Ticker, month, and year
  # Select necessary columns and reorder the columns
  df2 <- left_join(df, df2_SP500, by = c("Ticker", "month", "year")) %>% 
    select(date, Ticker, price, SP500, everything())
  
  return(df2)
}

# Example usage:
# combined_df <- IGVI_Combine_SP500(input_df, SP500_data)
