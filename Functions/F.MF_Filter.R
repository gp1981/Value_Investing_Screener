# Author: gp1981

# PURPOSE: Filter companies in Stock_List_data

# DISCLAIMER: This code is provided as is, without any guarantees or warranties. Use at your own risk.

# INPUT: Stock_List_data - a data frame containing the list of stock tickers
#       
#        

# OUTPUT: A list containing the Profile data for each company filtered

MF_Filter <- function(Stock_List_data, country, marketCap_limit_lower_M) {
  
  # List of European Union country abbreviations
  eu_countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", 
                    "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", 
                    "RO", "SK", "SI", "ES", "SE")
  
  # Replace "EU" with the list of EU countries
  new_country <- country
  if ("EU" %in% country) {
    new_country <- c(new_country[new_country != "EU"], eu_countries)
  }
  
  Stock_List_data <- Stock_List_data %>%  
    filter(
      country %in% new_country, # Filter by provided country
      !str_detect(companyName, 'Depositary'),  # Remove American Depository Certificate
      !str_detect(industry, "^(Bank|General Utilities|Insurance|Mortgage|REIT|Asset Management|Financial|Regulated)"), # Remove Bank, Insurance, and financials
      marketCap > marketCap_limit_lower_M*1e06
  )



return(Stock_List_data)

}
