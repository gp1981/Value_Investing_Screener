# Author: gp1981
# Purpose: Calculate Earnings Yield (EY) and Return on Capital (ROC) for companies and rank them according to Greenblatt's Magic Formula
# Disclaimer: This code is provided as-is without any warranty.

# INPUT: Financialmodellingprep API key
# OUTPUT: This function returns the data.frame of all financial information on stocks
#         including Earnings Yield and ROC and ranking

# 01 - Sourcing necessary files and libraries ----
source('Functions/Setup.R')         # Sourcing necessary libraries

# 02 - Inputs required ----
last_business_date <-as.Date("2025-10-17") # update here the last business date 
period <- "quarter"
period_limit <- 48
date_filename <- gsub("-", "", last_business_date)
# Lower limit market cap for Magic Formula
marketCap_limit_lower_M <- 1000
# Upper limit market cap for Magic Formula
marketCap_limit_upper_M <- 20000
# Market cap steps to retrieve data from magic formula
marketCap_step <- 1000

# 03 - API Download  ----
# Store the API key securely (you only need to do this once)
# keyring::key_set("API_FMP_KEY")
# keyring::key_set("MF_username")
# keyring::key_set("MF_password")

# Retrieve the API key
API_Key <- keyring::key_get("API_FMP_KEY")
Stock_List_data<-API_StockList(API_Key)
FX_rates_USD_df <- API_FX_rate(API_Key)
save(FX_rates_USD_df, file = paste0("Output/Data/FX_rates_USD_df_", date_filename, ".RData"), compress = "bzip2")

## 03.1 - Export stock list ----
Export_excel_StockList_data(Stock_List_data)

## 03.2 - Retrieve companies details and filter companies suitable for Magic Formula ----
Stock_List_data <- API_Profile(Stock_List_data, API_Key)
save(Stock_List_data, file = paste0("Output/Data/Stock_List_data_", date_filename, ".RData"), compress = "bzip2")
country <- c("US", "CA", "EU", "GB")
Stock_List_data <- MF_Filter(Stock_List_data, country, marketCap_limit_lower_M)

## 03.2 - Retrieve companies fundamentals ----
FinancialsMetricsProfile <- API_QFMP(Stock_List_data,API_Key,period, period_limit)
save(FinancialsMetricsProfile, file = paste0("Output/Data/", date_filename, ".RData"), compress = "bzip2")

# 04 - Greenblatt's ranking Calculation ----
## 04.1 - Aggregate stock fundamentals datatables ----
ROC_EY_v1_CACL <- Reduce_FinancialsMetricsProfile(FinancialsMetricsProfile, FX_rates_USD_df)

## 04.2 - Calculate Greenblatt ranking ----
ROC_EY_v1_CACL <- ROC_EY_Greenblatt_v1_CACL(ROC_EY_v1_CACL)

## 04.3 - Combine with S&P500 constituents ----
# ROC_EY_v1_CACL <- Combine_SP500(ROC_EY_v1_CACL, DF_SP500_all_FQ)

## 04.4 - Calculate Ratios and Graham parameters ----
ROC_EY_v1_CACL <- Ratios_Graham_MoS(ROC_EY_v1_CACL) 

## 04.5 - Calculate Statistics on companies valuation ----
ROC_EY_v1_CACL <- Maintenance_CAPEX(ROC_EY_v1_CACL)
ROC_EY_v1_CACL <- OwnerEarnings(ROC_EY_v1_CACL)
ROC_EY_v1_CACL <- CAGR_Equity(ROC_EY_v1_CACL)
ROC_EY_v1_CACL <- FCF_Negative(ROC_EY_v1_CACL)
ROC_EY_v1_CACL <- Multipliers(ROC_EY_v1_CACL)

# 05 - Last 4 Fiscal Quarters (FQ) ----
# DF_last_FQ <- DataFrame_last_FQ(ROC_EY_v1_CACL, mktCap_limit_lower_M, country)
DF_last_FQ <- DataFrame_last_FQ(ROC_EY_v1_CACL, mktCap_limit_lower_M, country)

## 05.1 - Match with data from Greenblatt Top 30-50 ----
DF_last_FQ_G <- Add_Top_Greenblatt(DF_last_FQ, last_business_date, marketCap_limit_lower_M, 
                                   marketCap_limit_upper_M, marketCap_step)

## 05.2 - Prepare output and export files ----
DF_last_FQ_Output_GIG_Full_List_Greenblatt <- Print_Output_GIG_Top50_Greenblatt(DF_last_FQ_G)
DF_last_FQ_GIG_Performance  <- Screener_Performance(DF_last_FQ_Output_GIG_Full_List_Greenblatt)
DF_last_FQ_GIG_Performance
Export_excel_DF_output(DF_last_FQ_Output_GIG_Full_List_Greenblatt,DF_last_FQ_GIG_Performance)

# 06 - Visualize statistics ----
## 06.1 - Historical ----
ROC_EY_v1_CACL_Plot <- Plot.Hist(df = ROC_EY_v1_CACL, 
                                 fundamentals = "Returns & Margins", 
                                 input_Tickers = "RIO")



## 06.2 - Actual ----
### 06.2.1 - Earnings Yield vs Return on Invested Capital ----
Plot_EY_ROC <- Plot.PointsXY(
  DF_last_FQ_G,
  xdata = "Return.On.Capital",
  ydata = "Earnings.Yield.Greenblatt",
  ndatapoints = 50,
  x_scale_limits = c(0, 2),
  y_scale_limits = c(0, 2.5),
  color = "red",
  title = "Companies selection by EY and ROC"
)
Plot_EY_ROC

### 06.2,2 - Plot the price and Net Asset/Current/Cash value per share ----
Plot_BCC <- Plot_Book_Current_Cash(DF_last_FQ_G)
Plot_BCC

# (WIP) - Benchmarking
# Benchmarking_Statistics <- IGVI_Benchmarking(Statistics.Data)
