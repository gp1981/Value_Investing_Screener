# Author: gp1981
# Date: 24 Dec 2022
# Purpose: Calculate map EY and ROC
# Disclaimer: The code is provided as-is and the author takes no responsibility for any issues or damages arising from its use.
# INPUT: DF
# OUTPUT: Plot EY vs RoC

Plot.PointsXY <- function(DF, xdata, ydata, ndatapoints, x_scale_limits, y_scale_limits, color, title) {  
  
  library(ggplot2)
  library(ggthemes)
  
  # 01 - Plotting -----------------------------------------------------------
  
  p0 <- ggplot(DF, aes_string(x = xdata, y = ydata)) +
    geom_point() +
    geom_point(data = DF[1:ndatapoints,], aes_string(x = xdata, y = ydata, color = shQuote(color))) +
    geom_text(data = DF[1:ndatapoints,], aes(label = Ticker), size = 2, vjust = -1) +
    scale_y_continuous(labels = scales::percent, limits = y_scale_limits) +
    scale_x_continuous(labels = scales::percent, limits = x_scale_limits) +
    labs(title = title)
  
  # 02 - Add theme ----------------------------------------------------------
  
  # Theme The Economist
  theme_set(theme_economist_white())
  
  # 03 - Prepare output -----------------------------------------------------
  
  ChartXY <- p0 + theme(plot.title = element_text(size = rel(1)),
                        legend.title = element_text(size = rel(0.1)),
                        legend.position = "top")
  
  return(ChartXY)
}
