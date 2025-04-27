# Author: gp1981
# Date: 13 Apr 2024
# Purpose: export file in excel
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.


library(openxlsx)

Export_excel_DF_output <- function(DF1, DF2 ) {
  
  ## Create workbook
  wb <- createWorkbook()
  
  ## Add worksheets
  addWorksheet(wb, "Data")
  addWorksheet(wb, "Performance")
  
  # Write DF1 to "Data" worksheet if provided
    writeDataTable(
      wb,
      "Data",
      x = as.data.frame(DF1),
      colNames = TRUE,
      tableStyle = "TableStyleLight9",
      tableName = "Data_Full_List"
    )
  
  # Write DF2 to "Performance" worksheet if provided
    writeDataTable(
      wb,
      "Performance",
      x = as.data.frame(DF2),
      colNames = TRUE,
      tableStyle = "TableStyleLight9",
      tableName = "Performance_Screener"
    )
  
  # Save workbook
  saveWorkbook(wb,
               file = paste0("Output/Data/ALL_DF_last_FQ_GIG_", date_filename, "_", rep(mktCap_limit_lower_M), ".xlsx"),
               overwrite = TRUE)
}

Export_excel_MF_data <- function(DF1) {
  
  ## Create workbook
  wb <- createWorkbook()
  
  ## Add worksheets
  addWorksheet(wb, "Data")
  
  # Write DF1 to "Data" worksheet if provided
  
    writeDataTable(
      wb,
      "Data",
      x = as.data.frame(DF1),
      colNames = TRUE,
      tableStyle = "TableStyleLight9",
      tableName = "Data_Full_List"
    )
  # Save workbook
  saveWorkbook(wb,
               file = paste0("Output/Data/MF_data_", date_filename, "_", rep(mktCap_limit_upper_M), ".xlsx"),
               overwrite = TRUE)
  # Check https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf
}

Export_excel_StockList_data <- function(DF1, DF2) {
  
  ## Create workbook
  wb <- createWorkbook()
  
  ## Add worksheets
  addWorksheet(wb, "Stock")

  
  # Write DF1 and DF2 to worksheet if provided
  
  writeDataTable(
    wb,
    "Stock",
    x = as.data.frame(DF1),
    colNames = TRUE,
    tableStyle = "TableStyleLight9",
    tableName = "Data_Stocks"
  )
  
  # Save workbook
  saveWorkbook(wb,
               file = paste0("Output/Data/Stock_List_data_", date_filename, ".xlsx"),
               overwrite = TRUE)
  # Check https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf
}

