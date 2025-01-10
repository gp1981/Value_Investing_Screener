# Author: gp1981
# Purpose: Retrieve stock data for US securities from the Financial Modeling Prep API and filter based on financial statements
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# API_StockList_US: Retrieve stock data for US securities from the Financial Modeling Prep API
# Input: API_Key (API key for accessing the API)
# Output: Data frame containing ticker symbols, company names, stock prices, and industry information for US securities with financial statements

API_StockList_US <- function(API_Key) {
  # Prepare URL for accessing stock list API
  Stock_List_path <- "https://financialmodelingprep.com/api/v3/stock/list?apikey="
  Stock_List_path <- paste0(Stock_List_path, API_Key)
  
  # Download stock list data via API
  Stock_List_data <- fromJSON(Stock_List_path)
  
  # Download financial statement symbol lists via API
  Financial_Statement_Symbol_List <- "https://financialmodelingprep.com/api/v3/financial-statement-symbol-lists?apikey="
  Financial_Statement_Symbol_List <- paste0(Financial_Statement_Symbol_List, API_Key)
  Financial_Statement_Symbol_List <- fromJSON(Financial_Statement_Symbol_List)
  
  # Convert JSON data into data frames
  Stock_List_data <- as.data.frame(Stock_List_data)
  Financial_Statement_Symbol_List <- as.data.frame(Financial_Statement_Symbol_List)
  
  # Rename column names
  names(Stock_List_data)[1] <- "Ticker"
  names(Stock_List_data)[2] <- "CompanyName"
  names(Stock_List_data)[3] <- "stock.price.today"
  Stock_List_data$Industry <- ""
  colnames(Financial_Statement_Symbol_List)[1] <- "Ticker"
  
  # Merge stock data with financial statement symbol list to include only securities with financial statements
  Stock_List_data <- semi_join(Stock_List_data, Financial_Statement_Symbol_List, by = "Ticker")
  
  # Filter stocks based on exchangeShortName
  Stock_List_data <- Stock_List_data %>%
    filter(exchangeShortName %in% c("AMEX", "ETF", "NASDAQ", "NYSE", "OTC"))
  
  return(Stock_List_data)
}
