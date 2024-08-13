# install.packages("quantmode")
# install.packages("PerformanceAnalytics")
# install.packages("dplyr")
# install.packages("scales")
# install.packages("data.table")
# install.packages("curl")

# LAOD LIBRARIES
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(scales)
library(data.table)
library(curl)
library(dplyr)
library(TTR)

# MACD

## Load data
getSymbols("SPY", from = "1993-1-29", to = Sys.Date())

macd <- MACD(Cl(SPY), nFast = 12, nSlow = 26, nSig = 9, maType = "SMA")

chartSeries(SPY, theme = chartTheme("white"))
addTA(macd[, 1], on = 1, col="blue")
addTA(macd[, 2], on = 1, col="red")
addTA(macd[, 1] - macd[, 2], type = "h", col="green")


# MACD Histogram
macdHist <- macd[, 1] - macd[, 2]
chartSeries(macdHist, theme = chartTheme("white"))
addTA(macdHist, on = 1, col = "green")

# MACD STRATEGY : BACK TESTING

# Create signals: 1 for buy, -1 for sell (or 0 for no action)
spy_signals <- ifelse(macd$macd > macd$signal, 1, ifelse(macd$macd < macd$signal, -1, 0))

# Convert to xts with the correct date index
spy_signals_xts <- xts(spy_signals, order.by = index(Cl(SPY)))

# Check signals and lengths
summary(spy_signals_xts)
print(length(spy_signals_xts))  # Should match length(Cl(SPY))


# Calculate daily returns
spy_daily_returns <- dailyReturn(Cl(SPY))
spy_daily_returns

# Align signals with daily returns
aligned_signals <- spy_signals_xts[index(spy_daily_returns)]
aligned_signals[is.na(aligned_signals)] <- 0  # Set NA signals to 0 (no action)
aligned_signals

# Calculate strategy returns
strategy_returns <- aligned_signals * spy_daily_returns
strategy_returns

# Cumulative returns
cumulative_returns <- cumprod(1 + strategy_returns) - 1

# Sharpe Ratio
sharpe_ratio <- SharpeRatio(strategy_returns, Rf = 0.01 / 252, FUN = "StdDev")

# Plot Performance Summary
charts.PerformanceSummary(strategy_returns, main = "MACD Strategy Performance")

# Print results
print(paste("Sharpe Ratio:", sharpe_ratio))




