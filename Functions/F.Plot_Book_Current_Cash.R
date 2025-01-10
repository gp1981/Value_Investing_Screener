# Author: gp1981
# Purpose: plot net book, net current, net cash value against price of the stocks
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.

# INPUT: DF_last_FQ_G
# OUTPUT: Plot Boxplot Book-Value, Current-Asset, Cash-Asset

Plot_Book_Current_Cash <- function(DF_last_FQ_G) {  
  
  library(ggplot2)
  library(dplyr)
  library(ggpubr)
  library(forcats)
  
  # 01 - Dataframe calculations ----------------------------------------------
  
  DF_Myscreener_TOP50 <- DF_last_FQ_G %>%
    filter(ID_Rank.Combined.EY_ROC.Greenblatt <= 50 &
             between(Net.Book.Value.per.Share, -400, 400) &
             between(Net.Current.Asset.Value.per.Share, -400, 400) &
             between(Net.Cash.Asset.Value.per.Share, -400, 400)) %>%
    mutate(Diff.Stock.Price_Current.Asset.Value = as.numeric(price) - as.numeric(Net.Current.Asset.Value.per.Share)) %>%
    arrange(desc(Diff.Stock.Price_Current.Asset.Value))
  
  DF_Myscreener_TOP50 <- DF_Myscreener_TOP50 %>%
    ungroup() %>%
    mutate(Ticker = fct_reorder(Ticker, desc(Diff.Stock.Price_Current.Asset.Value)))
  
  # 02 - Plotting -----------------------------------------------------------
  
  # Primary plot with lines and error bars for Net Book, Current, and Cash values
  p9 <- ggplot(DF_Myscreener_TOP50, aes(x = Ticker, y = Net.Current.Asset.Value.per.Share, group = 1)) +
    geom_point(size = 1.5, color = "#1f78b4") +
    geom_errorbar(aes(ymax = Net.Book.Value.per.Share, ymin = Net.Cash.Asset.Value.per.Share), 
                  width = 0.2, color = "#a6cee3", size = 0.7) +
    geom_point(aes(x = Ticker, y = as.numeric(price)), size = 2, color = "#e31a1c") +
    labs(
      title = "Comparison of Book, Current, and Cash Value per Share vs. Stock Price",
      y = "Value per Share (USD)", 
      x = NULL,
      caption = "Source: My Screener | Data: DF_last_FQ_G"
    ) +
    scale_colour_manual(name = "Indicators", values = c("Stock Price" = "#e31a1c")) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
    )
  
  # Secondary plot for Margin of Safety
  p10 <- ggplot(DF_Myscreener_TOP50, aes(x = Ticker, y = as.numeric(MoS), fill = "Margin of Safety")) +
    geom_bar(stat = "identity", width = 0.5, color = "#b2df8a", fill = "#33a02c") +
    labs(
      title = "Margin of Safety (Liquidation Value) for Top 50 Stocks",
      y = "Liquidation Value - Stock Price (USD)",
      x = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
    )
  
  # 03 - Prepare output -----------------------------------------------------
  
  p13 <- ggarrange(p9, p10, ncol = 1, nrow = 2, align = "v", heights = c(2, 1.5))
  return(p13)
}
