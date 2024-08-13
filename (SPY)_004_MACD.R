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
getSymbols("SPY", from = "1993-1-01", to = Sys.Date())

macd <- MACD(Cl(SPY), nFast = 12, nSlow = 26, nSig = 9, maType = "SMA")

chartSeries(SPY, theme = chartTheme("white"))
addTA(macd[, 1], on = 1, col="blue")
addTA(macd[, 2], on = 1, col="red")
addTA(macd[, 1] - macd[, 2], type = "h", col="green"")

