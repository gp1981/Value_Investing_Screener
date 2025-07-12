# Author: Gabriele Picheo - 16 Nov. 2024

# INPUT: DF with all fundamentals and maintenance capex
# OUTPUT: 

#   - Earnings.Power.Greenwald = avg.Op.Margin x Revenue x (1- avg. tax rate) + (depreciation - Maintenance.CAPEX)x0.5x(1- avg. tax rate) + adjustments (Greewald)

#.  - EVA = NOPAT - K 


OwnerEarnings <- function(df){
  
  # Calculation Earning Power Greenwald
  df <- df %>% 
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(
      
      # Calculate the cumulative sum of revenue, SG&A, R&D, other expenses, capex, depreciation, income tax, and full equity
      sum_Revenue = rollapply(revenue, width = period_limit, FUN = function(x) sum(x, na.rm = TRUE), align = "left", partial = TRUE),
      
      sust_Revenue.TTM = rollapply(revenue, width = 4, FUN = function(x) sum(x, na.rm = TRUE), align = "left", partial = TRUE),
      
      sum_SGA = rollapply(sellingGeneralAndAdministrativeExpenses, width = period_limit, FUN = function(x) sum(x, na.rm = TRUE), align = "left", partial = TRUE),
      
      sum_RD = rollapply(researchAndDevelopmentExpenses, width = period_limit, FUN = function(x) sum(x, na.rm = TRUE), align = "left", partial = TRUE),
      
      sum_Other_Expenses = rollapply(otherExpenses, width = period_limit, FUN = function(x) sum(x, na.rm = TRUE), align = "left", partial = TRUE),
      
      sum_Operating_Income = rollapply(operatingIncome, width = period_limit, FUN = function(x) sum(x, na.rm = TRUE), align = "left", partial = TRUE),
      
      sum_Capex = rollapply(capitalExpenditure, width = period_limit, FUN = function(x) sum(x, na.rm = TRUE), align = "left", partial = TRUE),
      
      sum_Income_Tax = rollapply(incomeTaxExpense, width = period_limit, FUN = function(x) sum(x, na.rm = TRUE), align = "left", partial = TRUE),
      
      sum_Income_Before_Tax = rollapply(incomeBeforeTax, width = period_limit, FUN = function(x) sum(x, na.rm = TRUE), align = "left", partial = TRUE)
      
    ) %>% 
    ungroup()
  
  df <- df %>% 
    mutate(
      avg.SGA_pct = sum_SGA / sum_Revenue,
      avg.RD_pct = sum_RD / sum_Revenue,
      avg.Other_Expenses_pct = sum_Other_Expenses / sum_Revenue,
      avg.Operating_Income_pct = sum_Operating_Income / sum_Revenue,
      avg.Capex_pct  = sum_Capex / sum_Revenue,
      avg.Income_Tax_pct = sum_Income_Tax / sum_Income_Before_Tax
    )
  
  # Calculate adjustments
  df <- df %>% 
    mutate(
      adj.SGA = ((sellingGeneralAndAdministrativeExpenses  / revenue) - avg.SGA_pct) * sust_Revenue.TTM,
      adj.RD = ((researchAndDevelopmentExpenses  / revenue) - avg.RD_pct) * sust_Revenue.TTM,
      adj.Other_Expenses = ((otherExpenses  / revenue) - avg.Other_Expenses_pct) * sust_Revenue.TTM,
      adj.Restoring_Assets = (depreciationAndAmortization - annualised.maintenance_capex/4),
      Earning.Power = (sust_Revenue.TTM * avg.Operating_Income_pct + adj.SGA + adj.RD + adj.Other_Expenses + adj.Restoring_Assets)*(1- avg.Income_Tax_pct)
    ) %>% 
    ungroup()
  
  
  # Calculation of Owner Earnings
  df  <- df %>% 
    mutate( 
      Owner.Earnings.Buffet = 
        netIncome + (depreciationAndAmortization - annualised.maintenance_capex/4) + 
        stockBasedCompensation + changeInWorkingCapital + deferredIncomeTax,
      
      Owner.Earnings.Buffet.IGVI = 
        netIncome + (depreciationAndAmortization - annualised.maintenance_capex/4) + 
        changeInWorkingCapital + deferredIncomeTax,
      
      Owner.Earnings.IGVI = 
        netCashProvidedByOperatingActivities - stockBasedCompensation - annualised.maintenance_capex/4 * 1.1,
    ) 
  
  # Calculating output: Earning Power, Owner Earnings, TTM and per Share
  df <- df %>% 
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(
      Earning.Power.TTM = rollapply(Earning.Power,
                                    width = 4, FUN = function(x) sum(x, na.rm = TRUE), align = "left", fill = NA)
    ) %>% 
    ungroup()
  
  df <- df %>% 
    group_by(Ticker) %>% 
    arrange(desc(date)) %>% 
    mutate(
      Earning.Power.per.Share.TTM = Earning.Power.TTM / outstandingShares,
      
      Owner.Earnings.Buffet.TTM = rollapply(Owner.Earnings.Buffet,
                                            width = 4, FUN = function(x) sum(x, na.rm = TRUE), align = "left", fill = NA),
      
      Owner.Earnings.Buffet.per.Share.TTM = Owner.Earnings.Buffet.TTM / outstandingShares,
      
      Owner.Earnings.Buffet.IGVI.TTM = rollapply(Owner.Earnings.Buffet.IGVI,
                                                 width = 4, FUN = function(x) sum(x, na.rm = TRUE), align = "left", fill = NA),
      
      Owner.Earnings.Buffet.IGVI.per.Share.TTM = Owner.Earnings.Buffet.IGVI.TTM / outstandingShares,
      
      Owner.Earnings.IGVI.TTM = rollapply(Owner.Earnings.IGVI,
                                          width = 4, FUN = function(x) sum(x, na.rm = TRUE), align = "left", fill = NA),
      
      Owner.Earnings.IGVI.per.Share.TTM = Owner.Earnings.IGVI.TTM / outstandingShares,
      
      marketCap.per.Share_LocalFX = marketCap_LocalFX / outstandingShares
      
    ) %>% 
    ungroup()
  
  return(df)
} 
