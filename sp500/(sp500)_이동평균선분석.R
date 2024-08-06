# install.packages("quantmod")
# install.packages("PerformanceAnalytics")

# Load library
library(quantmod)
library(PerformanceAnalytics)

# Load Data
getSymbols("^GSPC", src="yahoo", from="1991-01-01", to= Sys.Date())

tail(GSPC, 10)

# Visualization of S&P500 SMA

graphics.off()

chartSeries(GSPC, 
            name="S&P500 index", 
            type="line", 
            theme=chartTheme("white"), 
            TA="addTA(Cl(GSPC))")

addSMA(n =50, col = "blue")
addSMA(n = 200, col = "red")


# Visualization only moving averages
graphics.off()

sma_50 = SMA(Cl(GSPC), n=50)
sma_200 = SMA(Cl(GSPC), n=200)

plot(index(sma_50), coredata(sma_50), type = "l", col = "blue",
     main = "50-day and 200-day Moving Averages",
     xlab = "Date", ylab = "Value",
     xaxt = 'n')  # 기본 x축 레이블 제거
lines(index(sma_200), coredata(sma_200), col = "red")
