# Author: gp1981
# Date: 13 Apr 2024
# Purpose: Load all libraries and files
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# Import required libraries
packages <- c("httr","jsonlite","tidyverse", "openxlsx", "lubridate","tidyquant",
              "ggthemes","ggplot2","openxlsx","dplyr","zoo","ggpubr","foreach", "progress",
              "plotly", "keyring")

for (package in packages) {
  if (!(package %in% installed.packages())) {
    install.packages(package)
  }
  
  # Load the package
  library(package, character.only = TRUE)
}

# 02 - Functions to source ----
source('Functions/F.API_StockList.R')
source('Functions/F.API_StockList_US.R')
source('Functions/F.API_QFMP.R')
source('Functions/F.API_Profile.R')
source('Functions/F.MF_Filter.R')
source('Functions/F.ROC_EY_Greenblatt_v1_CACL.R')
source('Functions/F.DataFrame_last_FQ.R')
source('Functions/F.DataFrame_all_FQ.R')
source('Functions/F.Ratios_Graham_MoS.R')
source('Functions/P.PointsXY.R')
source('Functions/F.Plot_Book_Current_Cash.R')
source('Functions/F.Reduce_FinancialsMetricsProfile.R')
source('Functions/P.Hist.R')
source('Functions/F.Multipliers.R')
source('Functions/F.FCF_Negative.R')
source('Functions/F.CAGR_Equity.R')
source('Functions/F.Maintenance_CAPEX.R')
source('Functions/F.OwnerEarnings.R')
source('Functions/F.Print_Output_GIG_Top50_Greenblatt.R')
source('Functions/F.Print_Output_GIG_Top300.R')
source('Functions/F.Print_Output_GIG_Full_List.R')
source('Functions/F.Add_Top_Greenblatt.R')
source('Functions/F.Screener_Performance.R')
source('Functions/F.API_SP500_Historical.R')
source('Functions/F.Excess_Cash.R')
source('Functions/F.CombineSP500.R')
source('Functions/F.Import_MF_data.R')
source('Functions/F.Export_excel.R')

