# install.packages("quantmode")
# install.packages("PerformanceAnalytics")

# LOAD LIBRARY
library(quantmod)
library(PerformanceAnalytics)

analysis_moving_avg <- function(ticker_code, start_date){
  
  # LOAD DATA
  getSymbols(ticker_code, src = "yahoo", from = start_date)
  df <- get(ticker_code)

  # ADD A COLUMN OF SMA60
  df$SMA60 <- SMA(Cl(df), 60)
  # ADD A COLUMN OF SMA200
  df$SMA200 <- SMA(Cl(df), 200)
  
  # CHECK BULL OR BEAR MARKET
  df$MarketTrend <- ifelse(df$SMA60 - df$SMA200 > 0, TRUE, FALSE) # BULL MARKET = 1, BEAR MARKET = 0

  # CHCEK CROSSOVER
  # GOLDEN CROSS 0 -> 1
  df$GoldenCross <- ifelse(df$MarketTrend == TRUE & lag(df$MarketTrend, 1) == FALSE, 1, 0)
  # DEATH CROSS 1 -> 0
  df$DeathCross <- ifelse(df$MarketTrend == FALSE & lag(df$MarketTrend, 1) == TRUE, 1, 0)
  
  # GET THE DATES OF GOLDEN CROSS
  print(index(df[df$GoldenCross == 1, ]))
  
  # GET THE DATES OF DEAD CROSS
  print(index(df[df$DeathCross == 1, ]))

  return(df)
}
