# install.packages("quantmode")
# install.packages("PerformanceAnalytics")

# LOAD LIBRARY
library(quantmod)
library(PerformanceAnalytics)

visualization_moving_avg <- function(ticker_code, start_date){
  
  # LOAD DATA
  getSymbols(ticker_code, src="yahoo", from=start_date, to=Sys.Date())
  df <- get(ticker_code)
  
  # VISUALIZATION
    graphics.off()
  
    ## cALCUATE SMA
      cl_price <- Cl(df)
      sma_60 <- SMA(cl_price, n=60)
      sma_200 <- SMA(cl_price, n=200)
    
    ## BASIC PLOTTING
      plot(index(df), coredata(cl_price), type='l', col='black',
           main = "Moving Averages - SPY : 60, 200",
           xlab = "Year", ylab="Value",
           xaxt = 'n')
      
    ## ADD 'YEAR' LABEL
      years <- format(index(df), "%Y")
      unique_years <- unique(years)

      yearly_first_indexes <- sapply(unique_years, function(y) {
        min(which(years == y))
        })

      axis(1, 
           at=index(SPY)[yearly_first_indexes], 
           labels=unique_years, 
           las=2, 
           cex.axis=0.7, 
           tick=TRUE)

      
    ## ADD LEGEND
      legend("topleft", 
             legend=c("60-day SMA", "200-day SMA", "SNP500"), 
             col=c("green3", "orange", "black"), 
             lty=1)
      
      
    ## PLOT OTHER LINES
      lines(index(sma_60), coredata(sma_60), col = 'green3')
      lines(index(sma_200), coredata(sma_200), col = 'orange')
      
      
  # SELECT CROSSOVERS
      crossovers <- which(diff(sign(sma_60 - sma_200)) !=0)
      cross_dates <- index(sma_60)[crossovers]

  # PLOT CROSSOVER points
  # Identify crossover points
  golden_cross <- which(diff(sign(sma_60 - sma_200)) == 2) + 1
  dead_cross <- which(diff(sign(sma_60 - sma_200)) == -2) + 1

  # Plot Golden cross
  if (length(golden_cross) > 0) {
    points(index(sma_60)[golden_cross], sma_60[golden_cross], col="red3", pch=19, cex=0.5)
    abline(v=index(sma_60)[golden_cross], col="red3", lty=2)
  }
  
  
  # Plot Dead cross
  if (length(dead_cross) > 0) {
    points(index(sma_60)[dead_cross], sma_60[dead_cross], col="blue3", pch=19, cex=0.5)
    abline(v=index(sma_60)[dead_cross], col="blue3", lty=2)
    
  }
}
