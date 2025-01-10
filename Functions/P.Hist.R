# Author: gp1981
# Date: 01 Oct 2021
# Purpose: Generate statistic tables for various ratios based on input data
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# Input: ROC_EY_v1_CACL - Dataframe containing financial data for various companies and ratios
# Output: Additional variables

Plot.Hist <- function(df, fundamentals = "", input_Tickers = NULL) {
  
  # 01 - Prepare inputs -----------------------------------------------------
  
  # Predefined sets of variables
  predefined_sets <- list(
    "Returns & Margins" = c("operatingProfitMargin", "netProfitMargin", 
                            "returnOnEquity", "returnOnAssets", "Return.On.Capital"),
    "Multipliers" = c(
      "MktCap_EV_IGVI.Op.Assets", "MktCap_EV", "Debt_EV_IGVI.Op.Assets", 
      "CurrentAssets_EV_IGVI.Op.Assets", "CurrentAssets_EV", "NonCurrentAssets_EV_IGVI.Op.Assets", 
      "NonCurrentAssets_EV", "TotalAssets_EV_IGVI.Op.Assets", "totalAssets_EV", 
      "ExcessCash_EV_IGVI.Op.Assets", "ExcessCash_EV", "NetWorkingCapital_EV_IGVI.Op.Assets", 
      "NetWorkingCapital_EV"
    )
  )
  
  # Determine selected variables
  if (fundamentals %in% names(predefined_sets)) {
    selected_variables <- predefined_sets[[fundamentals]]
  } else {
    selected_variables <- fundamentals  # Assume input is a vector of specific variables
  }
  
  # 02 - Supporting functions -----------------------------------------------
  
  # Compute summary statistics
  stat_quantile <- function(x) {
    qs <- quantile(x, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    data.frame(Min = qs[1], Q1 = qs[2], Median = qs[3], Avg = mean(x, na.rm = TRUE), Q3 = qs[4], Max = qs[5])
  }
  
  # Replace outliers using the IQR method
  replace_outliers_with_na <- function(df, column_name, multiplier = 7) {
    Q1 <- quantile(df[[column_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[column_name]], 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    lower_bound <- Q1 - multiplier * IQR_value
    upper_bound <- Q3 + multiplier * IQR_value
    df[[column_name]] <- ifelse(
      df[[column_name]] < lower_bound | df[[column_name]] > upper_bound, 
      NA, 
      df[[column_name]]
    )
    return(df)
  }
  
  
  # 03 - Prepare data for visualization --------------------------------------
  
  # Filter and select relevant columns early and add fiscal year
  df <- df %>% 
    select(Ticker, date, sector, industry, all_of(selected_variables)) %>%
    mutate(fiscal_year = year(date))
  
  # Apply the outlier replacement to selected variables
  for (var in selected_variables) {
    df <- replace_outliers_with_na(df, var)
  }
  
  # Convert to long format
  df_clean_long <- df %>%
    pivot_longer(cols = all_of(selected_variables),
                 names_to = "variable", values_to = "value")
  
  # Count data points by fiscal_year and sector
  df_count <- df_clean_long %>%
    group_by(fiscal_year, variable, sector) %>%
    summarise(count = sum(!is.na(value)), .groups = "drop")
  
  # Calculate the threshold based on median of total_count per year
  threshold_datapoints <- df_count %>%
    group_by(fiscal_year) %>%
    summarise(total_count = sum(count), .groups = "drop") %>%
    summarise(q1 = quantile(total_count, 0.5)) %>%  # Use median of yearly total_count
    pull(q1)
  
  valid_years <- df_count %>%
    group_by(fiscal_year) %>%
    summarise(total_count = sum(count), .groups = "drop") %>%
    filter(total_count >= threshold_datapoints) %>%
    pull(fiscal_year)
  
  df_clean_long_filtered <- df_clean_long %>%
    filter(fiscal_year %in% valid_years)
  
  # Prepare data for highlighting Tickers
  if (!is.null(input_Tickers)) {
    df_ticker <- df_clean_long %>%
      filter(Ticker %in% input_Tickers) %>%
      mutate(quarter = quarter(date)) %>%  # Add quarter information
      group_by(fiscal_year, variable, sector, Ticker, quarter) %>%
      slice_max(order_by = date, n = 1) %>%  # Keep the most recent value for each quarter
      ungroup() %>%
      mutate(Ticker_Quarter = paste(Ticker, "Q", quarter, sep = "-"))  # Add Ticker-Quarter info for legend
  }
  
  unique_sectors <- unique(df_ticker$sector)
  
  # Create Title for Sector or Industry
  plot_title <- "Variable Values Across Years (By Sector)"
  
  # Visualization
  plot.sector <- ggplot(df_clean_long_filtered, aes(
    x = factor(fiscal_year),
    y = value,
    fill = sector
  )) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.4, color = "black", position = position_dodge(width = 0.8)) +
    {
      if (!is.null(df_ticker)) {
        geom_point(
          data = df_ticker,
          aes(
            x = factor(fiscal_year),
            y = value,
            color = Ticker_Quarter,
            shape = Ticker_Quarter
          ),
          size = 2,
          inherit.aes = FALSE
        )
      }
    } +
    facet_wrap(~ variable, ncol = 3, scales = "free_y") +
    labs(
      title = plot_title,
      x = "Fiscal Year",
      y = "Value",
      color = "Ticker-Quarter",  # Legend shows Ticker + Quarter
      shape = "Ticker-Quarter"  # Legend shows Ticker + Quarter
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      strip.text = element_text(size = 10),
      legend.position = "top",
      legend.box = "horizontal",
      plot.margin = margin(10, 10, 10, 10)
    ) +
    scale_fill_brewer(palette = "Set3") +
    scale_color_manual(values = c("red", "blue", "green", "purple")) + 
    scale_shape_manual(values = c(17, 15, 18, 16))
  
  if (length(unique_sectors) == 1) {
    input_sector <- unique_sectors
    
    industries <- unique(df %>% filter(sector == input_sector & Ticker %in% input_Tickers) %>% pull(industry))
    
    # Create Title for Sector or Industry
    plot_title <- "Variable Values Across Years (By Industry)"
    
    df_by_industry <- df %>%
      filter(industry %in% industries) %>%
      select(Ticker, date, industry, all_of(selected_variables)) %>%
      mutate(fiscal_year = year(date)) %>%
      pivot_longer(cols = all_of(selected_variables), 
                   names_to = "variable", values_to = "value")
    
    plot.industry <- ggplot(df_by_industry, aes(
      x = factor(fiscal_year),
      y = value,
      fill = industry
    )) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.4, color = "black", position = position_dodge(width = 0.8)) +
      {
        if (!is.null(df_ticker)) {
          geom_point(
            data = df_ticker,
            aes(
              x = factor(fiscal_year),
              y = value,
              color = Ticker_Quarter,
              shape = Ticker_Quarter
            ),
            size = 2,
            inherit.aes = FALSE
          )
        }
      } +
      facet_wrap(~ variable, ncol = 3, scales = "free_y") +
      labs(
        title =  plot_title ,
        x = "Fiscal Year",
        y = "Value",
        color = "Ticker-Quarter",
        shape = "Ticker-Quarter"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 10),
        legend.position = "top",
        legend.box = "horizontal",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      scale_fill_brewer(palette = "Set3") + 
      scale_color_manual(
        values = c("red", "blue", "green", "purple")
      ) +
      scale_shape_manual(
        values = c(17, 15, 18, 16)
      )
  } else {
    plot.industry <- NULL
  }
  
  # Output
  return(list(
    plot.sector = plot.sector,
    plot.industry = plot.industry,
    df = df,
    df_ticker = df_ticker,
    df_by_industry = if (exists("df_by_industry")) df_by_industry else NULL
  ))
}

