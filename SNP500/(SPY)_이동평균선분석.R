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
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(scales)


# I. LOAD DATA

## LOAD SPY IN YAHOO
getSymbols("SPY", src='yahoo', from="1991-01-01", to=Sys.Date())
SPY 
tail(SPY)

## DATA PROCESSING
  ### SMA60
  SPY$SMA60 <- SMA(Cl(SPY), n=60)
  ### SMA200
  SPY$SMA200 <- SMA(Cl(SPY), n=200)
  ### MARKET TREND
  SPY$MarketTrend <- ifelse(SPY$SMA60 > SPY$SMA200, 1, 0)
  ### GOLDEN CROSS
  SPY$GoldenCross <- ifelse(SPY$MarketTrend == 1 & lag(SPY$MarketTrend == 0), 1, 0)
  ### DEATH CROSS
  SPY$DeadCross <- ifelse(SPY$MarketTrend == 0 & lag(SPY$MarketTrend == 1), 1, 0)
  
  GoldenCrossDates <- index(SPY[SPY$GoldenCross == 1])
  DeadCrossDates <- index(SPY[SPY$DeadCross == 1])
                        
## VISUALIZATION
  graphics.off()
  ### PLOT(ADD 'YEAR' LABEL)
    plot(index(SPY), SPY$SPY.Close, type='l', col='black', main="SPY", xlab="YEAR", ylab="Close Price", xaxt = 'n')
    years <- format(index(SPY), "%Y")
    unique_years <- unique(years)
  
    yearly_first_indexes <- sapply(unique_years, function(y) {min(which(years == y))})
  
    axis(1, at=index(SPY)[yearly_first_indexes], labels=unique_years, las=2,  cex.axis=0.7, tick=TRUE)
  
  
  lines(index(SPY), SPY$SMA60, col='blue', lwd=2)
  lines(index(SPY), SPY$SMA200, col='orange', lwd=2)
  if (length(GoldenCrossDates) > 0) {
    points(GoldenCrossDates, SPY$SMA60[GoldenCrossDates], col="red3", pch=19, cex=0.5)
    abline(v=as.Date(GoldenCrossDates), col="red3", lty=2)
  }
  if (length(DeadCrossDates) > 0) {
    points(DeadCrossDates, SPY$SMA60[DeadCrossDates], col="blue3", pch=19, cex=0.5)
    abline(v=as.Date(DeadCrossDates), col="blue3", lty=2)
  }
  
# II. SET SEED MONEY

initial_asset <- function(seed_money=100){
  asset <- c(seed_money, 0)
  names(asset) <- c("cash", "stock")
  return(asset)
}

asset <- initial_asset()
asset


# III. TRADE

## BUY
buy_stock <- function(asset, date, dt, price_col, ratio){
  
  cash_to_buy <- asset["cash"] * ratio
  stocK_price <- dt[[price_col]][dt$date == as.Date(date)]
  purchase_amount <- cash_to_buy %/% stocK_price
  asset["cash"] <- asset["cash"] - purchase_amount * stocK_price
  asset["stock"] <- asset["stock"] + purchase_amount
  return(asset)
}

## SELL
sell_stock <- function(asset, date, dt, price_col, ratio){
  stock_amount <- as.numeric(asset["stock"])
  stock_to_sell <- floor(stock_amount * ratio)
  stocK_price <- dt[[price_col]][dt$date == as.Date(date)]
  asset["cash"] <- asset["cash"] + stock_to_sell * stocK_price
  asset["stock"] <- asset["stock"] - stock_to_sell
  return(asset)
}

require(data.table)
spy_df <- as.data.frame(as.data.table(SPY))
spy_df$date <- as.Date(spy_df$index)


## CALCULATE EVALUATION AMOUNT
eval_asset <- function(asset, date, dt, price_col){
  stock_price <- dt[[price_col]][dt$date == as.Date(date)]
  return(asset["cash"] + asset["stock"] * stock_price)
}

## CALCULATION ASSET DISTRIBUTION RATIO
asset_distribution_ratio <- function(asset, date, dt, price_col){
  stock_price <- dt[[price_col]][dt$date == as.Date(date)]
  return(asset["stock"] * stock_price / eval_asset(asset, date, dt, price_col))
}

asset <- buy_stock(asset, "1993-01-29", spy_df, "SPY.Adjusted", 0.5)
asset <- sell_stock(asset, "1993-02-26", spy_df, "SPY.Adjusted", 0.6)
