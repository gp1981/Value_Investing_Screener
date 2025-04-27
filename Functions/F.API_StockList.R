# Author: gp1981
# Purpose: Retrieve stock data for US and Canadian securities from the Financial Modeling Prep API and filter based on financial statements and S&P500 inclusion
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# Input: API_Key (API key for accessing the API)
# Output: Data frame containing ticker symbols, company names, stock prices, industry information, and S&P500 inclusion for US and Canadian securities with financial statements

API_StockList <- function(API_Key) {
  # Download financial statement symbol lists via API
  Financial_Statement_Symbol_List <- "https://financialmodelingprep.com/stable/financial-statement-symbol-list?apikey="
  Financial_Statement_Symbol_List <- paste0(Financial_Statement_Symbol_List, API_Key)
  Financial_Statement_Symbol_List <- fromJSON(Financial_Statement_Symbol_List)
  Financial_Statement_Symbol_List <- as.data.frame(Financial_Statement_Symbol_List)
  Stock_List_data <- Financial_Statement_Symbol_List 
  rm(Financial_Statement_Symbol_List)
  
  # Rename column names
  names(Stock_List_data)[1] <- "Ticker"
  names(Stock_List_data)[2] <- "CompanyName"
  
  return(Stock_List_data)
}
