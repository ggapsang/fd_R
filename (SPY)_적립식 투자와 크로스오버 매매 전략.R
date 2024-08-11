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

## LOAD SPY IN YAHOO
getSymbols("SPY", src='yahoo', from="1991-01-01", to=Sys.Date())
tail(SPY)

# II. STRATEGY : TREND FOLLWING AND CROSSOVER TRADE

short_ma <- SMA(Cl(SPY), n=120)
long_ma <- SMA(Cl(SPY), n=250)

signals <- rep(NA, NROW(Cl(SPY))) 

# CROSSOVER STATEGY
## CHECK THE GODLNE CROSS -> LONG POSITION
## CHECK THE DEAD CROSS -> SHORT POSITION

for (i in 2:(NROW(Ad(SPY)) - 1)) {
  # 이동평균 값이 NA가 아닌지 먼저 확인
  if (!is.na(short_ma[i]) && !is.na(long_ma[i]) && !is.na(short_ma[i-1]) && !is.na(long_ma[i-1])) {
    
    # 골든크로스 발생 시, 다음 날 매수 신호
    if (short_ma[i] > long_ma[i] && short_ma[i-1] <= long_ma[i-1]) {
      signals[i + 1] <- 1
      
      # 데드크로스 발생 시, 다음 날 매도 신호
    } else if (short_ma[i] < long_ma[i] && short_ma[i-1] >= long_ma[i-1]) {
      signals[i + 1] <- 0
      
      # 유지 상태 (신호가 없다면 이전 신호를 그대로 유지)
    } else if (is.na(signals[i + 1])) {
      signals[i + 1] <- signals[i]
    }
  }
}

# 골든 크로스와 데드 크로스를 계산할 수 없을 경우 포지션을 매수로 잡는다
signals[is.na(signals)] <- 1


## SET INITIAL CAPITAL
initial_capital <- 100
monthly_invest <- 100
capital <- initial_capital

## CALC POSITION
position <- Lag(signals, k = 1)
returns <- dailyReturn(Ad(SPY))

portfolio_value <-  xts(rep(NA, NROW(Ad(SPY))), order.by=index(returns))
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

charts.PerformanceSummary(returns)




