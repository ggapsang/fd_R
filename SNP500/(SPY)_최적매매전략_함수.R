# install.packages("quantmode")
# install.packages("PerformanceAnalytics")
# install.packages("dplyr")
# install.packages("scales")
# install.packages("data.table")
# install.packages("curl")

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

# 수익률 계산을 위한 함수
calculate_returns <- function(df, golden_cross, dead_cross) {
  
  df$returns <- 0
  df$invest.amount <-0
  
  for (g_idx in golden_cross) {
    
    df$invest.amount[df$date == as.Date(g_idx)] <- df$SPY.Adjusted[df$date == as.Date(g_idx)] # 투자금(주식 구입 비용)
    subsequent_dead_cross <- dead_cross[dead_cross > g_idx][1]  # 골든 크로스 이후 첫 번째 데드 크로스
    
    if (!is.na(subsequent_dead_cross)) {
      df$returns[df$date == as.Date(subsequent_dead_cross)] <- df$SPY.Adjusted[df$date == as.Date(subsequent_dead_cross)] - df$SPY.Adjusted[df$date == as.Date(g_idx)]  # 차익 계산
    }
  }
  return(df)
}

# SPY ETF에 대한 기본 데이터프레임 조작
get_spy_df <- function(short, long, tk_code, start_date, end_date){

# LOAD DATA
getSymbols(tk_code, src='yahoo', from=start_date, to=end_date)
xts_dt <- get(tk_code)
xts_dt

# CALC LONG/SHORT SMA
xts_dt$SMA.Short <- SMA(Cl(xts_dt), n=short)
xts_dt$SMA.Long <- SMA(Cl(xts_dt), n=long)

# CHECK BULL OR BEAR
xts_dt$MarketTrend <- ifelse(xts_dt$SMA.Short - xts_dt$SMA.Long >0 ,TRUE, FALSE)

# GOLDEN CROSS
xts_dt$GoldenCross <- ifelse(xts_dt$MarketTrend == TRUE & lag(xts_dt$MarketTrend, 1) == FALSE, 1, 0)

# DEAD CROSS
xts_dt$DeadCross <- ifelse(xts_dt$MarketTrend == FALSE & lag(xts_dt$MarketTrend, 1) == TRUE, 1, 0)

# GET DATE (INDEX)
golden_cross_idx <- index(xts_dt[xts_dt$GoldenCross == 1,])
dead_cross_idx <- index(xts_dt[xts_dt$DeadCross == 1,])

# CONVERT DATAFRAME
require(data.table)
df <- as.data.frame(as.data.table(xts_dt))
df$date <- as.Date(df$index)

return(df)
}

# 이동평균선 교차 매매 전략
sma_crossover_strategy <- function(short, long, tk_code, start_date, end_date){
  
  df <- get_spy_df(short, long, tk_code, start_date, end_date)
  
  result_df <- calculate_returns(df, golden_cross_idx, dead_cross_idx)
  
  total_invest <- round(sum(result_df$invest.amount), 2)
  total_returns <- round(sum(result_df$returns), 2)
  revenue_rate <- round((total_invest + total_returns)/total_invest, 2)
  
  print(paste("short :", short, "long :", long, "invest :", total_invest,  "returns :", total_returns, "revenue.rates :", revenue_rate))
  
  all_crosses <- c(golden_cross_idx, dead_cross_idx)
  filtered_df <- result_df[result_df$date %in% all_crosses, ]
  
  results <- c(total_invest, total_returns, revenue_rate)
  return(results)
}

# INITIALIZE ASSET
init_asset <- function(seedmoney=100){
  asset <- c(0, 0)
  names(asset) <- c("stock", "cash")
  asset <- c(0, seedmoney) 
  return(asset)
} 

asset <- init_asset()
asset

# BUY/SELL
buy <- function(asset, date, df){
  # 주식 구입
  purchase_stock_amount <- c %/% df$SPY.Adjusted[df$date == as.Date(date)]
  asset["stock"] <- asset["stock"] + purchase_stock_amount
  
  # 현금 차감
  asset["cash"] <- asset["cash"] - purchase_stock_amount * df$SPY.Adjusted[df$date == as.Date(date)]
  return(asset)
}

sell <- function(asset, date, df){
  # 주식 매도
  sales_amount_cash <- asset["stock"] * df$SPY.Adjusted[df$date == as.Date(date)]
  asset["cash"] <- asset["cash"] + sales_amount_cash
  
  # 주식 차감
  asset["stock"] <- 0
  return(asset)
}

# SAINVGS INVESTMENT STRATEGY
  # dead cross가 발생하면 20일 간격으로 매입(현금이 떨어지거나 다음 골든 크로스가 오기 전까지)
  # golden cross가 발생하면 20일 간격으로 매도(주식이 떨어지거나 다음 데드 크로스가 오기 전까지)

sainvgs_investment_strategy <- function(short, long, tk_code, start_date, end_date, asset){

  df <- get_spy_df(tk_code, start_date, end_date)
  
  # dead cross가 발생하면 20일 간격으로 매입(현금이 떨어지거나 다음 골든 크로스가 오기 전까지)
  
  for (date in 1:nrow(df)) {
    if (df[,"cash"] > 0) {
      asset <- buy(asset, date, df)
    }
  }
  
  # golden cross가 발생하면 20일 간격으로 매도(주식이 떨어지거나 다음 데드 크로스가 오기 전까지)
  
  
  
  return(df)
}
