# install.packages("quantmode")
# install.packages("PerformanceAnalytics")
# install.packages("dplyr")
# install.packages("scales")
# install.packages("data.table")
# install.packages("curl")

# IMPORT LIBRARY
library(data.table)
library(curl)
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(scales)

################################################################################

# CALCLATE RETURNS
calculate_returns <- function(df, golden_cross, dead_cross) {
  
  df$returns <- 0
  df$invest.amount <-0
  
for (g_idx in golden_cross) {
  
  # 골든 크로스 이후 첫번째 데드 크로스 찾기
  subsequent_dead_cross <- dead_cross[dead_cross > g_idx][1]  # 골든 크로스 이후 첫 번째 데드 크로스
  df$invest.amount[df$date == as.Date(g_idx)] <- df$SPY.Adjusted[df$date == as.Date(g_idx)] # 투자금(주식 구입 비용)
  
  if (!is.na(subsequent_dead_cross)) {
    df$returns[df$date == as.Date(subsequent_dead_cross)] <- df$SPY.Adjusted[df$date == as.Date(subsequent_dead_cross)] - df$SPY.Adjusted[df$date == as.Date(g_idx)]  # 차익 계산
    }
  }
  return(df)
}

# STRATEGY : SMA CROSSOVER
sma_crossover_strategy <- function(short, long, tk_code, start_date, end_date){
  
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
  
  result_df <- calculate_returns(df, golden_cross_idx, dead_cross_idx)
  
  total_invest <- sum(result_df$invest.amount)
  total_returns <- sum(result_df$returns)
  revenue_rate <- (total_invest + total_returns)/total_invest
  
  print(paste("short :", short, "long :", long, "invest :", total_invest,  "returns :", total_returns, "revenue.rates :", revenue_rate))
  
  all_crosses <- c(golden_cross_idx, dead_cross_idx)
  filtered_df <- result_df[result_df$date %in% all_crosses, ]
  
  results <- c(total_invest, total_returns, revenue_rate)
  return(results)
}

# 최적 장단기 조합 찾기
short_terms <- c(5, 10, 20, 40, 60, 80)
long_terms <- c(10, 20, 40, 60, 80, 120, 200, 240)

short_terms_days <- c()
long_terms_days <- c()
invest_amount <- c()
total_revenues <- c()
total_revenues_rate <-c()

for (short_term in short_terms){
  for (long_term in long_terms){
    if (short_term < long_term){
      results <- sma_crossover_strategy(short_term, long_term, "SPY", "1993-01-29", Sys.Date())
      short_terms_days <- c(short_terms_days, short_term)
      long_terms_days <- c(long_terms_days, long_term)
      invest_amount <- c(invest_amount, results[1]) 
      total_revenues <- c(total_revenues, results[2])
      total_revenues_rate <-c(total_revenues_rate, results[3])
    }
  }
}

# TEST RESULT
test_report_for_smacross_strategy = data.frame("short"=short_terms_days,
                                               "long"=long_terms_days,
                                               "invest"=round(as.numeric(invest_amount), 2),
                                               "returns"=round(as.numeric(total_revenues),2),
                                               "rates"=round(as.numeric(total_revenues_rate), 2))

head(test_report_for_smacross_strategy)
tail(test_report_for_smacross_strategy)
summary(test_report_for_smacross_strategy)

################################################################################

# STRATEGY : SAVINGS INVESTMENT
# 데드 크로스 이후 분할 매수(20일마다). 골든 크로스 이후 분할 매도(20일마다)

# INITIALIZE ASSET
init_asset <- function(seedmoney=100){
  asset <- c(0, 0)
  names(asset) <- c("stock", "cash")
  asset <- c(0, seedmoney) 
  return(asset)
} 

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

tk_code <- "SPY"
start_date <- "1993-01-01"
end_date <- Sys.Date()
short = 120
long = 250

# LOAD DATA
getSymbols(tk_code, src='yahoo', from=start_date, to=end_date)
xts_dt <- get(tk_code)

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

df

################################################################################


sma_crossover_startegey_savings_invest <- function(short, long, tk_code, start_date, end_date){
  
  # LOAD DATA
  getSymbols(tk_code, src='yahoo', from=start_date, to=end_date)
  xts_dt <- get(tk_code)
  
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

}