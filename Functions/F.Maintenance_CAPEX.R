# Author: gp1981
# Date: 24 Dec 2022
# Purpose: Calculate the maintenance CAPEX
# INPUT: ROC_EY_v1_CACL
# OUTPUT: DF.Maintenance.CAPEX

Maintenance_CAPEX <- function(df) {
  # Step 1: Initial Calculations - Cumulative Sums
  # - Group data by ticker to apply cumulative sums starting from the oldest quarter.
  df <- df %>%
    group_by(Ticker) %>%
    arrange(desc(date)) %>%
    mutate(
      # Reverse cumulative sum of capital expenditure and depreciation to ensure trailing values are from the oldest date.
      total.capital_expenditure = rev(cumsum(rev(capitalExpenditure))),
      total.depreciation_amortization = rev(cumsum(rev(depreciationAndAmortization)))
    ) %>%
    ungroup()
  
  # Step 2: Rolling Averages and Growth CAPEX Calculations
  # - Group by ticker and apply rolling operations for ratios and growth calculations.
  df <- df %>%
    group_by(Ticker) %>%
    arrange(desc(date)) %>%
    mutate(
      # Calculate the ratio of revenue to capital employed.
      ratio.revenue_FixedAsset = coalesce(revenue / (propertyPlantEquipmentNet + otherNonCurrentAssets),0),
      
      # Calculate the trailing average of the ratio over the period_limit.
      avg_revenue = rollapply(
        revenue, width = period_limit, FUN = mean, align = "left", partial = TRUE
      ),
      
      avg_ratio.revenue_FixedAsset = rollapply(
        ratio.revenue_FixedAsset, width = period_limit, FUN = mean, align = "left", partial = TRUE
      ),
      
      # Calculate the ratio to avg revenue.
      avg_ratio.revenue = coalesce(revenue / avg_revenue,0),
      
      avg_ratio.revenue = ifelse(avg_ratio.revenue > 1, avg_ratio.revenue, 1),
      
      # Calculate the difference in revenue between the first and last periods within the period_limit.
      revenue_diff = rollapply(
        revenue, width = period_limit, FUN = function(x) -1 * (last(x) - first(x)), align = "left", partial = TRUE
      ),
      
      # Set any negative revenue_diff to zero (representing no growth in revenue).
      revenue_diff = ifelse(revenue_diff < 0, 0, revenue_diff),
      
      # Compute growth CAPEX as the product of average ratio and revenue difference.
      growth_capex = avg_ratio.revenue_FixedAsset * revenue_diff,
      
      # Calculate total maintenance CAPEX by subtracting growth CAPEX from total capital expenditure.
      total.maintenance_capex = (-1) * total.capital_expenditure - growth_capex,
      
      total.maintenance_capex = ifelse(total.maintenance_capex < 0, (-1) * total.capital_expenditure, total.maintenance_capex)
      
    ) %>%
    ungroup()
  
  # Step 3: Annualizing Maintenance CAPEX
  # - Calculate quarter distance from the oldest date and use it to annualize maintenance CAPEX.
  df <- df %>%
    group_by(Ticker) %>%
    arrange(date) %>%
    mutate(
      # Calculate quarter distance from the earliest date to estimate the length of each maintenance CAPEX period in years.
      quarter_distance = row_number()
    ) %>%
    arrange(desc(date)) %>%
    mutate(
      # Annualized maintenance CAPEX by dividing by quarter distance converted to years. Used revenue increase respect to avg as multiplying factor 
      annualised.maintenance_capex = (coalesce(total.maintenance_capex / (quarter_distance / 4),0) * avg_ratio.revenue)
      ) %>%
    ungroup() %>%
    
    # Step 4: Output Formatting
    # - Select specific columns for the final output.
    select(Ticker, date, annualised.maintenance_capex, capitalExpenditure, industry, sector, everything())
  
  return(df)
}
