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

source("(SPY)_001_Saving Investment and Crossover Stratgey.R")

# SHARP RATIO에 대한 실시간 추적. 미국 국채 수익률(10 - year Treasury Rate)과 비교하여 측정

# I. 미국 국채 수익률 데이터 획득

getSymbols("DGS10", from="1993-01-29", src = "FRED") # 10-year Treasury Rate
tail(DGS10)

print(summary(DGS10))

print(length(DGS10) == length(SPY))
print(length(DGS10))
print(length(SPY))

## DGS10의 경우 결측값도 있고, 같은 기간인데도 SPY보다 길이가 짧다
## FOWARD FILL
### 합리적인 방식은 미국 국채 수익률의 길이를 SPY와 같은 길이로 늘리는 것이다. 대신, 이렇게 해서 생기는 결측값은 장이 열리지 않았다고 판단하고 바로 그 앞의 값으로 채운다.
### 이런 방식은 금융 데이터에서 거래가 없는 날의 데이터를 처리할 때 자주 사용되며, 시계열의 연속성을 유지하는데 도움이 된다

DGS10_full <-  merge(DGS10, SPY, join = "right")





















