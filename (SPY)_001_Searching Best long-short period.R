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
getSymbols("SPY", src='yahoo', from="1993-01-29", to=Sys.Date())
tail(SPY)

## DATA PROCESSING
data_preprocessing <- function(data, short, long){
  data$SMA.Short <- SMA(Cl(data), n=short)
  data$SMA.Long <- SMA(Cl(data), n=long)
  data$MarketTrend <- ifelse(data$SMA.Short > data$SMA.Long, 1, 0)
  data$GoldenCross <- ifelse(data$MarketTrend == 1 & lag(data$MarketTrend) == 0, 1, 0)
  data$DeadCross <- ifelse(data$MarketTrend == 0 & lag(data$MarketTrend) == 1, 1, 0)
  return(data)
  }
  
SPY <- data_preprocessing(SPY, 60, 200)

## VISUALIZATION
graphics.off()
plot(index(SPY), SPY$SPY.Close, type='l', col='black', main="SPY", xlab="YEAR", ylab="Close Price", xaxt = 'n')
years <- format(index(SPY), "%Y")
unique_years <- unique(years)
  
yearly_first_indexes <- sapply(unique_years, function(y) {min(which(years == y))})
  
axis(1, at=index(SPY)[yearly_first_indexes], labels=unique_years, las=2,  cex.axis=0.7, tick=TRUE)
  
lines(index(SPY), SPY$SMA.Short, col='blue', lwd=2)
lines(index(SPY), SPY$SMA.Long, col='orange', lwd=2)
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

## INVESTMENT
invest <- function(asset, invest_amount){
  asset["cash"] <- asset["cash"] + invest_amount
  return(asset)
}


require(data.table)
spy_df <- as.data.frame(as.data.table(SPY))
spy_df$date <- as.Date(spy_df$index)
spy_df[is.na(spy_df)] <-0

## CALCULATE EVALUATION AMOUNT
eval_asset <- function(asset, date, dt, price_col){
  stock_price <- dt[[price_col]][dt$date == as.Date(date)]
  total_amount <- c(asset["cash"] + asset["stock"] * stock_price)
  names(total_amount) <- c("Total Amount")
  return(total_amount)
}

## CALCULATION ASSET DISTRIBUTION RATIO
asset_distribution_ratio <- function(asset, date, dt, price_col){
  stock_price <- dt[[price_col]][dt$date == as.Date(date)]
  evaluate_price <- c(asset["stock"] * stock_price / eval_asset(asset, date, dt, price_col))
  names(evaluate_price) <- c("Distribution Ratio")
  return(evaluate_price)
}


# IV. STRATEGY 1: TREND FOLLWING
trend_following <- function(asset, date, dt, price_col){
  for (i in 1:nrow(dt)){
    date <- dt$date[i]
    if (i > 1){
      if (dt$GoldenCross[i] == 1){
        asset <- buy_stock(asset, date, dt, price_col, 1)
      } else if (dt$DeadCross[i] == 1){
        asset <- sell_stock(asset, date, dt, price_col, 1)
      }
    }
  }
  results <- c(asset, eval_asset(asset, date, dt, price_col), asset_distribution_ratio(asset, date, dt, price_col))
  names(results) <- c("cash", "stock", "Total Amount", "Distribution Ratio")
  return(results)
}


asset <- initial_asset()
results <- trend_following(asset, "2024-08-09", spy_df, "SPY.Close")

## FIND THE BEST SHORT AND LONG PERIOD

short_temrs <- c(5, 10, 20, 40, 60, 80, 120)
long_termss <- c(10, 20, 40, 60, 80, 120, 200, 250)

col_short_terms <- c()
col_long_terms_days <- c()
col_invest_amount <- c()
col_cash <- c()
col_stock <- c()
col_total_amount <- c()
col_distribution_ratio <- c()

for (short_term in short_temrs){
  for (long_term in long_termss){
    if (short_term < long_term){
      SPY <- data_preprocessing(SPY, short_term, long_term)
      spy_df <- as.data.frame(as.data.table(SPY))
      spy_df$date <- as.Date(spy_df$index)
      spy_df[is.na(spy_df)] <- 0
      asset <- initial_asset()
      results <- trend_following(asset, "2024-08-09", spy_df, "SPY.Close")
      col_short_terms <- c(col_short_terms, short_term)
      col_long_terms_days <- c(col_long_terms_days, long_term)
      col_invest_amount <- c(col_invest_amount, results["stock"])
      col_cash <- c(col_cash, results["cash"])
      col_stock <- c(col_stock, results["stock"])
      col_total_amount <- c(col_total_amount, results["Total Amount"])
      col_distribution_ratio <- c(col_distribution_ratio, results["Distribution Ratio"])
    }
  }
}

results_df <- data.frame(col_short_terms, col_long_terms_days, col_invest_amount, col_cash, col_stock, col_total_amount, col_distribution_ratio)

## SUMMARY
summary(results_df)
best_results <- results_df[which.max(results_df$col_total_amount),]
print(best_results) 

top3_results <- results_df[order(results_df$col_total_amount, decreasing=TRUE),][1:3,]
print(top3_results)


## SAVINGS_INVESMENT
getSymbols("SPY", src='yahoo', from="1991-01-01", to=Sys.Date())
SPY <- data_preprocessing(SPY, 120, 250)
spy_df <- as.data.frame(as.data.table(SPY))
spy_df$date <- as.Date(spy_df$index)
spy_df[is.na(spy_df)] <- 0
asset <- initial_asset()
for (i in 1:nrow(spy_df)){
  date <- spy_df$date[i]
  if (i > 1){
    if (i %% 20 == 0){
      asset <- invest(asset, 100)
    }
    
    if (spy_df$GoldenCross[i] == 1){
      asset <- buy_stock(asset, date, spy_df, "SPY.Close", 1)
    } else if (spy_df$DeadCross[i] == 1){
      asset <- sell_stock(asset, date, spy_df, "SPY.Close", 1)
    }
  }
}
results <- c(asset, eval_asset(asset, date, spy_df, "SPY.Close"), asset_distribution_ratio(asset, date, spy_df, "SPY.Close"))
names(results) <- c("cash", "stock", "Total Amount", "Distribution Ratio")
results
































