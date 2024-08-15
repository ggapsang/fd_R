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

tk_code <- "QQQ"
start_date <- "1993-01-01"
end_date <- Sys.Date()

getSymbols(tk_code, from=start_date, to=end_date)
macd <- MACD(Cl(QQQ), nFast=12, nSlow=26, nSig=9, maType="EMA")

macd

chartSeries(QQQ, theme=chartTheme('white'))
addTA(macd[, 1], on=1, col="blue")
addTA(macd[, 2], on=1, col='green')

# MACD HISTOGRAM
macdHist <- macd[, 1] - macd[, 2]
chartSeries(macdHist, theme=chartTheme('white'))
addTA(macdHist, on=1, col='green')

# MACD STRATEGY : BACK TESTING

## CRETATE SIGNALS : 1 FOR BUY, -1 FOR SELL (OR - FOR NO ACTION)
qqq_signlas <- ifelse(macd$macd > macd$signal, 1, -ifelse(macd$macd < macd$signal, -1, 0))

## CONVERT TO XTS WITH THE CORRECT DATE INDEX
qqq_signals_xts <- xts(qqq_signlas, order.by=index(Cl(QQQ)))

## CHECK SIGNALS AND LENGTHS
summary(qqq_signals_xts)
print(length(qqq_signals_xts)) # should match length(Cl(QQQ))

## CACULATE DAILY RETURNS
qqq_daily_returns <- dailyReturn(Cl(QQQ))
qqq_daily_returns

## ALIGN SIGNALS WITH DAILY RETURNS
qqq_aligned_signals <- qqq_signals_xts[index(qqq_daily_returns)]
qqq_aligned_signals[is.na(qqq_aligned_signals)] <- 0 # SET NA signals to 0(no action)
qqq_aligned_signals

## CALCULATE STRATEGY RETURNS
qqq_strategy_returns <- qqq_aligned_signals * qqq_daily_returns
qqq_strategy_returns

## CUMULATIVE RETURNS
qqq_cumulative_returns <- cumprod(1 + qqq_strategy_returns) - 1

## SHARP RATIO
sharp_ratio <- SharpeRatio(qqq_strategy_returns, Rf = 0.01 / 252, FUN = "StdDev")

## PLOT PERFORMANCE SUMMARY
charts.PerformanceSummary(qqq_strategy_returns, main="MACD Strategy Performance")

print(paste("Sharp Ratio: ", sharp_ratio))




