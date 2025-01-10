# Author: Gabriele Picheo - 15 Jan 2021
# update 07/10/2021
# INPUT: DF
# OUTPUT:

# --->>> TO REPLACE F.Plot_Book_Current_Cash <<<<---

Plot.Bar <- function(DF, xdata, ydata, yprice, ymin, ymax, ndatapoints, title)
{  

# 01 - Plotting -----------------------------------------------------------

  
  p0 <- ggplot(DF, aes_string(x=xdata, y=ydata)) +  geom_line() + geom_point() +
    geom_errorbar(aes_string(ymax=shQuote(ymax),ymin=shQuote(ymin))) + 
    geom_point(aes_string(x = xdata, y = as.numeric(shQuote(yprice)), colour = "red")) +
  
  p1 <- p0 + geom_point(data = DF[1:ndatapoints,], 
                        aes_string(x=xdata , y=ydata, color = shQuote(color)))  +
    scale_y_continuous(labels=scales::percent, limits = y_scale_limits) +
    scale_x_continuous(labels=scales::percent, limits = x_scale_limits) 
  
  p2 <- p1 + geom_text(data = DF[1:ndatapoints,], mapping = aes(label = Ticker), 
                       size = 2, vjust=-1)
  
  p3 <- p2 + labs(title)

# 02 - Add theme ----------------------------------------------------------

  
  # Theme The Economist
  theme_set(theme_economist_white())

# 03 - Prepare output -----------------------------------------------------

  
  ChartXY <- p3 + theme(plot.title = element_text(size = rel(1)),
                   legend.title = element_text(size = rel(0.1)),
                   legend.position = "top"
  )
  return(ChartXY)
}
