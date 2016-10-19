RegressionPlots <- function(fit){
  
  # Extract fitted values from lm() object
  Fitted.Values <-  fitted(fit)
  
  # Extract residuals from lm() object
  Residuals <-  resid(fit)
  
  # Extract standardized residuals from lm() object
  Standardized.Residuals <- MASS::stdres(fit)  
  
  # Extract fitted values for lm() object
  Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x
  
  # Square root of abs(residuals)
  Root.Residuals <- sqrt(abs(Standardized.Residuals))
  
  # Calculate Leverage
  Leverage <- lm.influence(fit)$hat
  
  # Create data frame 
  # Will be used as input to plot_ly
  
  regMat <- data.frame(Fitted.Values, 
                       Residuals, 
                       Standardized.Residuals, 
                       Theoretical.Quantiles,
                       Root.Residuals,
                       Leverage)
  
  # Plot using Plotly
  
  # Fitted vs Residuals
  # For scatter plot smoother
  LOESS1 <- loess.smooth(Fitted.Values, Residuals)
  
  plt1 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Residuals vs Fitted Values", plot_bgcolor = "#e6e6e6", width = 1000)
  
  # QQ Pot
  plt2 <- regMat %>% 
    plot_ly(x = Theoretical.Quantiles, y = Standardized.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "line", name = "",
              line = list(width = 2)) %>% 
    
    layout(title = "Q-Q Plot", plot_bgcolor = "#e6e6e6")
  
  # Scale Location
  # For scatter plot smoother
  LOESS2 <- loess.smooth(Fitted.Values, Root.Residuals)
  
  plt3 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Root.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Scale Location", plot_bgcolor = "#e6e6e6", width = 1000)
  
  # Residuals vs Leverage
  # For scatter plot smoother
  LOESS3 <- loess.smooth(Leverage, Residuals)
  
  plt4 <- regMat %>% 
    plot_ly(x = Leverage, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Leverage vs Residuals", plot_bgcolor = "#e6e6e6")
  
  plt = list(plt1, plt2, plt3, plt4)
  return(plt)
}


t <- seq(0,3, length=52*4)
DemA <- c(rep(100,13), rep(200,13), rep(300,13), rep(400,13), rep(500,13), rep(650,13), rep(900,13), 
          rep(1000,13), rep(500,52), rep(250,52))
DemB <- c(rep(500,13), rep(700,13), rep(1000,13), rep(1250,13), rep(1500,13), rep(2000,13), rep(1000,13), 
          rep(900,13), rep(450,52), rep(100,52))
DemC <- c(rep(0,13), rep(0,13), rep(0,13), rep(100,13), rep(850,13), rep(1250,13), rep(1500,13), 
          rep(1250,13), rep(600,52), rep(300,52))



library(plotly)
plot_ly(x = ~t, y= ~DemA, name = "demand A", type = 'scatter', mode='lines') %>%
  add_trace(y= ~DemB, name = "demand B", mode='lines') %>%
  add_trace(y= ~DemC, name='demand C', mode='lines') %>%
  layout(title= "Forecasted Weekly Demand", 
         yaxis=list(title="Demand in units"),
         xaxis=list(title="Time from Q1 '17"))

linInt <- lm(DemA[1:(52*2)] ~ t[1:(52*2)])
plt = RegressionPlots(linInt)
fit2 <- lm( DemA[(52*2):(52*4)] ~ t[(52*2):(52*4)] )
lw <- loess(DemA ~ t)
subplot(plt, nrows=2, widths = c(0.475,0.475), heights=c(0.5,0.5), margin=0.05)


plot_ly(x = ~t) %>%
  add_lines(y= ~DemA, name = "demand A", line=list(shape="linear")) %>%
  add_trace(x=t[1:(52*2)], y=fitted(linInt), name="Demand A to peak", mode='lines') %>%
  add_trace(x=t[(52*2):(52*4)], y=fitted(fit2), name="Demand A post-peak", mode='lines') %>%
  add_lines(y = ~fitted(loess(DemA ~ t)), name="Smoothed Demand")
  

