# Author: gp1981
# Date: 13 Apr 2024
# Purpose: Load Magic Formula data https://www.magicformulainvesting.com/
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

library(rvest)
library(tidyverse)
library(progress)
library(openxlsx)

Import_MF_data <- function(last_business_date, mktCap_limit_lower_M, mktCap_limit_upper_M, mktCap_step) {
  
  # 1 - Login and open session ----------------------------------------------
  
  # Ask for username and password
  MF_username <- keyring::key_get("MF_username")
  MF_password <- keyring::key_get("MF_password")
  
  # Create a session
  session <- session("https://www.magicformulainvesting.com/Account/LogOn")
  
  # Submit login form
  form <- html_form(session)[[1]]
  filled_form <- html_form_set(form, "Email" = MF_username, "Password" = MF_password)
  session <- session_submit(session, filled_form)
  
  # Check if login was successful
  response <- session$response
  if (response$status_code == 200 && session$url == "https://www.magicformulainvesting.com/Screening/StockScreening") {
    print("Login successful!")
  } else {
    stop("Login failed. Please check your username and password.")
  }
  
  # Reinitialize form with the session redirected to the screeningpage
  form <- html_form(session)[[1]]
  
  # Initialize an empty data frame to store the final results
  company_data <- data.frame()
  
  # Initialize progress bar
  pb <- progress_bar$new(format = "[:bar] :percent :elapsed", 
                         total = length(seq(mktCap_limit_lower_M, 
                                            mktCap_limit_upper_M, 
                                            by = mktCap_step)
                         )
  )
  
  
  # 2 - Extract data --------------------------------------------------------
  
  # Loop through market cap thresholds
  for (MinimumMarketCap in seq(mktCap_limit_lower_M, mktCap_limit_upper_M, by = mktCap_step)) {
    # # Increment progress bar
    pb$tick()
    
    # Submit form to get top 30
    filled_form_top30 <- html_form_set(form, "Select30" = "true", "MinimumMarketCap" = MinimumMarketCap)
    session <- session_submit(session, filled_form_top30)
    
    # Extract top 30 data
    company_data_top30 <- extract_company_data(session, last_business_date, MinimumMarketCap, "Top30")
    
    # Submit form to get top 50
    filled_form_top50 <- html_form_set(form, "Select30" = "false", "MinimumMarketCap" = MinimumMarketCap)
    session <- session_submit(session, filled_form_top50)
    
    # Extract top 50 data
    company_data_top50 <- extract_company_data(session, last_business_date, MinimumMarketCap, "Top50")
    
    # Merge company_data_top50 into company_data_top30 preserving the records in company_data_top30
    company_data_merged <- merge(company_data_top30, company_data_top50, by = c("Company_Name", "Ticker", "Market_Cap_Millions", "Price_From", "Most_Recent_Quarter_Data", "threshold_mktCap", "TopGreenblatt"), all = TRUE)
    
    company_data_merged_unique <- company_data_merged %>%
      group_by(Company_Name) %>% 
      arrange(TopGreenblatt) %>% 
      distinct(Company_Name, .keep_all = TRUE) %>% 
      ungroup()
    
    # Append to company_data 
    company_data <- rbind(company_data, company_data_merged_unique)
    
    # Pause for 0.2 second to avoid overloading the server
    Sys.sleep(0.2)
  }
  # 
  # Filter duplicate at higher threshold market cap
  company_data <- company_data %>%
    group_by(Company_Name) %>%
    arrange(threshold_mktCap) %>%
    distinct(Company_Name, .keep_all = TRUE) %>%
    ungroup()
  
  
  # Checking the final data
  print(company_data)
  
  #  3 - Formatting data -----------------------------------------------------
  
  # Convert Market_Cap_Millions column to numeric
  company_data$Market_Cap_Millions <- as.numeric(company_data$Market_Cap_Millions)
  
  # Convert Date columns to date format
  year <- format(last_business_date, "%Y")
  company_data$Price_From <- as.Date(paste0(year, "-", substr(company_data$Price_From, 1, 2), "-", substr(company_data$Price_From, 4, 5)))
  company_data$Most_Recent_Quarter_Data <- as.Date(paste0(year, "-", substr(company_data$Most_Recent_Quarter_Data, 1, 2), "-", substr(company_data$Most_Recent_Quarter_Data, 4, 5)))
  
  return(company_data)
}

# Function to extract company data from session
extract_company_data <- function(session, last_business_date, MinimumMarketCap, TopGreenblatt) {
  year <- format(last_business_date, "%Y")
  
  table <- session %>% html_nodes("table.screeningdata tbody")
  if (length(table) == 0) {
    message("No data available for market cap: ", MinimumMarketCap)
    return(data.frame())  # Return empty data frame
  }
  
  company_data <- table %>%
    html_nodes("tr") %>%
    map_df(~{
      tds <- html_nodes(.x, "td")
      if (length(tds) < 5) return(data.frame())  # Skip invalid rows
      data.frame(
        Company_Name = html_text(tds[1]),
        Ticker = html_text(tds[2]),
        Market_Cap_Millions = gsub(",", "", html_text(tds[3]), fixed = TRUE),
        Price_From = html_text(tds[4]),
        Most_Recent_Quarter_Data = html_text(tds[5]),
        threshold_mktCap = MinimumMarketCap,
        TopGreenblatt = TopGreenblatt,
        stringsAsFactors = FALSE
      )
    })
}

