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

## DATA PROCESSING
data_preprocessing <- function(data, short, long){
  data$SMA.Short <- SMA(Cl(data), n=short)
  data$SMA.Long <- SMA(Cl(data), n=long)
  data$MarketTrend <- ifelse(data$SMA.Short > data$SMA.Long, 1, 0)
  data$GoldenCross <- ifelse(data$MarketTrend == 1 & lag(data$MarketTrend) == 0, 1, 0)
  data$DeadCross <- ifelse(data$MarketTrend == 0 & lag(data$MarketTrend) == 1, 1, 0)
  return(data)
}

# INITIALIZE ASSET
initial_asset <- function(seed_money=100){
  asset <- c(seed_money, 0)
  names(asset) <- c("cash", "stock")
  return(asset)
}

# BUY/SELL
buy_stock <- function(asset, date, dt, price_col, ratio){
  
  cash_to_buy <- asset["cash"] * ratio
  stocK_price <- dt[[price_col]][dt$date == as.Date(date)]
  purchase_amount <- cash_to_buy %/% stocK_price
  asset["cash"] <- asset["cash"] - purchase_amount * stocK_price
  asset["stock"] <- asset["stock"] + purchase_amount
  return(asset)
}

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

## TREND FOLLOWING STRATEGY
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
