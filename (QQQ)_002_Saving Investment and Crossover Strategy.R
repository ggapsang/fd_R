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
library(scales)

# I. LOAD DATA
## LOAD QQQ DATA

getSymbols("QQQ", src="yahoo", from="1999-03-10", to=Sys.Date())
tail(QQQ)

# II. STRATEGY : TREND FOLLOWING AND CROSSOVER TRADE

short_ma <- SMA(Cl(QQQ), 60)
long_ma <- SMA(Cl(QQQ), 120)

signals <- rep(NA, NROW(Cl(QQQ)))

# CROSSOVER STATEGY
## CHECK THE GODLNE CROSS -> LONG POSITION
## CHECK THE DEAD CROSS -> SHORT POSITION

for (i in 2:(NROW(Ad(QQQ)) - 1)){
  
  # 이동평균(MA)가 NA 값이 아닌지 먼저 확인
  if (!is.na(short_ma[i]) && !is.na(long_ma[i]) && !is.na(short_ma[i-1]) && !is.na(long_ma[i-1])){
    
    # 골든 크로스 발생 시, 다음 날 매수 신호
    if (short_ma[i] > long_ma[i] && short_ma[i-1] < long_ma[i-1]){
      signals[i + 1] <- 1
    
    # 데드 크로스 발생시, 다음 날 매도 신호
    } else if (short_ma[i] < long_ma[i] && short_ma[i-1] > long_ma[i-1]){
      signals[i + 1] <- 0
    
    # 유지 상태(신호가 없다면 이전 신호를 그대로 유지)
    } else if (is.na(signals[i + 1])){
      signals[i + 1] <- signals[i]
    }
  }
}
## 골든크로스와 데드 크로스를 계산할 수 없을 경우(이평선이 60, 120이 누적되지 않음) 포지션을 매수로 잡는다
signals[is.na(signals)] <- 1

## SET INITIAL CAPITAL
initial_capital <- 100
monthly_invest <- 100
capital <- initial_capital

## CALC POSITION
position <-  Lag(signals, k = 1)
returns <- dailyReturn(Ad(QQQ))
portfolio_value <- xts(rep(NA, NROW(Ad(QQQ))), order.by=index(returns))

## portfolio_value is the key to portfolio tracking. After setting the initial capital, the value of the portfolio is calculated by reflecting the daily rate of return, and the result is recorded in the portfolio_value object.

portfolio_value[1] <- initial_capital

for (i in 2:NROW(returns)) {
  # 20일 간격으로 추가 투자
  if (i %% 20 == 0) {
    capital <- capital + monthly_invest
  }
  
  # 자본을 기반으로 새로운 포지션 평가
  
  capital <- capital * (1 + coredata(returns[i]) * coredata(position[i]))
  portfolio_value[i] <- capital
}

Rf <- 0.05
daily_rf <- Rf/252

annualized_sharpe <- SharpeRatio.annualized(returns, Rf = daily_rf, scale = 252)
print(annualized_sharpe)

charts.PerformanceSummary(returns)








