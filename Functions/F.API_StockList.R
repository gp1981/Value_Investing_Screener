# Author: gp1981
# Purpose: Retrieve stock data for US and Canadian securities from the Financial Modeling Prep API and filter based on financial statements and S&P500 inclusion
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# Input: API_Key (API key for accessing the API)
# Output: Data frame containing ticker symbols, company names, stock prices, industry information, and S&P500 inclusion for US and Canadian securities with financial statements

API_StockList <- function(API_Key) {
  # Prepare URL for accessing stock list API
  Stock_List_path <- "https://financialmodelingprep.com/api/v3/stock/list?apikey="
  Stock_List_path <- paste0(Stock_List_path, API_Key)
  
  # Download stock list data via API
  Stock_List_data <- fromJSON(Stock_List_path)
  
  # Download financial statement symbol lists via API
  Financial_Statement_Symbol_List <- "https://financialmodelingprep.com/api/v3/financial-statement-symbol-lists?apikey="
  Financial_Statement_Symbol_List <- paste0(Financial_Statement_Symbol_List, API_Key)
  Financial_Statement_Symbol_List <- fromJSON(Financial_Statement_Symbol_List)
  
  # Download S&P500 constituent list via API
  SP500_Symbol_list <- "https://financialmodelingprep.com/api/v3/sp500_constituent?apikey="
  SP500_Symbol_list <- paste0(SP500_Symbol_list, API_Key)
  SP500_Symbol_list <- fromJSON(SP500_Symbol_list)
  
  # Convert JSON data into data frames
  Stock_List_data <- as.data.frame(Stock_List_data)
  Financial_Statement_Symbol_List <- as.data.frame(Financial_Statement_Symbol_List)
  SP500_Symbol_list <- as.data.frame(SP500_Symbol_list)
  
  # Add a column to mark only the S&P500 stocks
  SP500_Symbol_list <- SP500_Symbol_list %>% 
    mutate(SP500 = 'S&P500')
  
  # Rename column names
  names(Stock_List_data)[1] <- "Ticker"
  names(Stock_List_data)[2] <- "CompanyName"
  names(Stock_List_data)[3] <- "stock.price.today"
  Stock_List_data$Industry <- ""
  colnames(Financial_Statement_Symbol_List)[1] <- "Ticker"
  colnames(SP500_Symbol_list)[1] <- "Ticker"
  
  # Merge stock data with financial statement symbol list to include only securities with financial statements
  Stock_List_data <- semi_join(Stock_List_data, Financial_Statement_Symbol_List, by = "Ticker")
  Stock_List_data <- left_join(Stock_List_data, SP500_Symbol_list, by = "Ticker")
  
  return(Stock_List_data)
}
