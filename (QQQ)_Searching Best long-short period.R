# install.packages("quantmod")
# install.packages("PerformanceAnalytics")

# LOAD LIBRARY
library(quantmod)
library(PerformanceAnalytics)

source("(My_Lib)_functions.R")

# LOAD DATA
getSymbols("QQQ", from="1999-03-10", to=Sys.Date())
tail(QQQ)

## DATA PROCESSING
data_preprocessing <- function(data, short, long){
  data$SMA.Short <- SMA(Cl(data), n=short)
  data$SMA.Long <- SMA(Cl(data), n=long)
  data$MarketTrend <- ifelse(data$SMA.Short > data$SMA.Long, 1, 0)
  data$GoldenCross <- ifelse(data$MarketTrend == 1 & lag(data$MarketTrend) == 0, 1, 0)
  data$DeadCross <- ifelse(data$MarketTrend == 0 & lag(data$MarketTrend) == 1, 1, 0)
  return(data)
}

QQQ <- data_preprocessing(QQQ, 60, 200)

GoldenCrossDates <- index(QQQ)[QQQ$GoldenCross == 1]
DeadCrossDates <- index(QQQ)[QQQ$DeadCross == 1]
GoldenCrossDates <- na.omit(GoldenCrossDates)
DeadCrossDates <- na.omit(DeadCrossDates)


## VISUALIZATION
graphics.off()
plot(index(QQQ), QQQ$QQQ.Close, type='l', col='black', main="QQQ", xlab="YEAR", ylab="Close Price", xaxt = 'n')
years <- format(index(QQQ), "%Y")
unique_years <- unique(years)

yearly_first_indexes <- sapply(unique_years, function(y) {min(which(years == y))})

axis(1, at=index(QQQ)[yearly_first_indexes], labels=unique_years, las=2,  cex.axis=0.7, tick=TRUE)

lines(index(QQQ), QQQ$SMA.Short, col='blue', lwd=2)
lines(index(QQQ), QQQ$SMA.Long, col='orange', lwd=2)
if (length(GoldenCrossDates) > 0) {
  points(GoldenCrossDates, QQQ$SMA.Short[GoldenCrossDates], col="red3", pch=19, cex=0.5)
  abline(v=GoldenCrossDates, col="red3", lty=2)
}
if (length(DeadCrossDates) > 0) {
  points(DeadCrossDates, QQQ$SMA.Short[DeadCrossDates], col="blue3", pch=19, cex=0.5)
  abline(v=DeadCrossDates, col="blue3", lty=2)
}

# II. SEARCHING BEST LONG/SHORT PERIOD
# 최적 장단기 조합 찾기
require(data.table)
qqq_df <- as.data.frame(as.data.table(QQQ))
qqq_df$date <- as.Date(qqq_df$index)
qqq_df[is.na(qqq_df)] <-0

# INITIALIZE ASSET
asset <- initial_asset()
results <- trend_following(asset, "2024-08-09", qqq_df, "QQQ.Close")

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
      QQQ <- data_preprocessing(QQQ, short_term, long_term)
      qqq_df <- as.data.frame(as.data.table(QQQ))
      qqq_df$date <- as.Date(qqq_df$index)
      qqq_df[is.na(qqq_df)] <- 0
      asset <- initial_asset()
      results <- trend_following(asset, "2024-08-09", qqq_df, "QQQ.Close")
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
getSymbols("QQQ", src='yahoo', from="1999-03-10", to=Sys.Date())
QQQ <- data_preprocessing(QQQ, 120, 250)
qqq_df <- as.data.frame(as.data.table(QQQ))
qqq_df$date <- as.Date(qqq_df$index)
qqq_df[is.na(qqq_df)] <- 0
asset <- initial_asset()
for (i in 1:nrow(qqq_df)){
  date <- qqq_df$date[i]
  if (i > 1){
    if (i %% 20 == 0){
      asset <- invest(asset, 100)
    }
    
    if (qqq_df$GoldenCross[i] == 1){
      asset <- buy_stock(asset, date, qqq_df, "QQQ.Close", 1)
    } else if (qqq_df$DeadCross[i] == 1){
      asset <- sell_stock(asset, date, qqq_df, "QQQ.Close", 1)
    }
  }
}
results <- c(asset, eval_asset(asset, date, qqq_df, "QQQ.Close"), asset_distribution_ratio(asset, date, qqq_df, "QQQ.Close"))
names(results) <- c("cash", "stock", "Total Amount", "Distribution Ratio")
results


