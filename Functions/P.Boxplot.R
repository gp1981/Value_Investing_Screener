# Author: gp1981
# Date: 24 Dec 2022
# Purpose: makes box plot on financials statistics 
# INPUT: Statistics
# OUTPUT: Boxplot

Plot.Boxplot <- function(Statistics, parameter) {
  library(ggplot2)
  
  parameter_data <- Statistics[Statistics$Ratio == parameter, ]
  
  Plot_Boxplot <- ggplot(parameter_data,
                         aes(y = Dataset,
                             x = Median,
                             xmin = Q1,
                             xmax = Q4)) +
    geom_boxplot(stat = "identity", show.legend = FALSE) +
    geom_point(data = parameter_data[1, ], aes(x = as.numeric(`Company Ratio`), color = "darkred"), size = 4, show.legend = FALSE) +
    labs(title = "Benchmark", x = parameter, y = "Dataset")
  
  return(Plot_Boxplot)
}
