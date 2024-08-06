# install.packages("quantmode")
# install.packages("PerformanceAnalytics")

# LOAD LIBRARY
library(quantmod)
library(PerformanceAnalytics)

# LOAD DATA
getSymbols("SPY", src = "yahoo", from = "1993-01-29")

tail(SPY)

# ADD A COLUMN OF SPY.SMA50
SPY$SPY.SMA50 <- SMA(Cl(SPY), 50)

# ADD A COLUMN OF SPY.SMA200
SPY$SPY.SMA200 <- SMA(Cl(SPY), 200)

# CHECK BULL OR BEAR MARKET
#SPY <- na.omit(SPY)
sum(is.na(SPY$SPY.SMA50))
sum(is.na(SPY$SPY.SMA200))
SPY$SPY.MarketTrend <- ifelse(SPY$SPY.SMA50 - SPY$SPY.SMA200 > 0, TRUE, FALSE) # BULL MARKET = 1, BEAR MARKET = 0

tail(SPY)

# CHECK CROSSOVER
# Golden crossover 0 -> 1
SPY$SPY.GoldenCross <- ifelse(SPY$SPY.MarketTrend == TRUE & lag(SPY$SPY.MarketTrend, 1) == FALSE, 1, 0)
# Death crossover 1 -> 0
SPY$SPY.DeathCross <- ifelse(SPY$SPY.MarketTrend == FALSE & lag(SPY$SPY.MarketTrend, 1) == TRUE, 1, 0)

tail(SPY)

# GET THE DATES OF GOLDEN CROSS
print(index(SPY[SPY$SPY.GoldenCross == 1, ]))

# GET THE DATES OF DEAD CROSS
print(index(SPY[SPY$SPY.DeathCross == 1, ]))
