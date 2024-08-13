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
print(length(returns))

## DGS10의 경우 결측값도 있고, 같은 기간인데도 SPY보다 길이가 짧다
## returns의 경우 길이가 제일 짧다
## FOWARD FILL
### 합리적인 방식은 미국 국채 수익률의 길이를 SPY와 같은 길이로 늘리거나, returns를 DSG10만큼 늘리는 것이다. 대신, 이렇게 해서 생기는 결측값은 장이 열리지 않았다고 판단하고 바로 그 앞의 값으로 채운다.
### 이런 방식은 금융 데이터에서 거래가 없는 날의 데이터를 처리할 때 자주 사용되며, 시계열의 연속성을 유지하는데 도움이 된다

# SPY 데이터와 DGS10 데이터의 인덱스를 맞추고 NA로 확장
returns_processed <- merge(DGS10, returns, all=TRUE)

# NA 값 forward fill 처리, 앞쪽 결측치 유지
returns_processed <- na.locf(returns_processed, na.rm = FALSE)

summary(returns_processed)
print(length(returns_processed))

missing_dates <- setdiff(index(returns), index(DGs10)) # SPY에 없는 인덱스 찾기
missing_dates_actual <- as.Date(missing_dates)
print(missing_dates_actual)
head(returns_processed)

returns_processed$DGS10_Daily_Return <- (1 + returns_processed$DGS10 / 100)^(1/252) - 1 ## 10년 수익률 국채의 1일 수익률 계산
head(returns_processed)

returns_processed <- returns_processed[, -which(names(returns_processed) == "DGS10")] ## 10년 수익률 칼럼은 드롭
head(returns_processed)

charts.PerformanceSummary(returns_processed)


portfolio_sd <- sd(returns_processed$daily.returns) # 포트폴리오 수익률의 표준편차 계산
riskfree_sd <- sd(returns_processed$DGS10_Daily_Return) # 무위험 수익률의 표준편차 계산

print(portfolio_sd)
print(riskfree_sd)


