# install.packages("quantmod")
# install.packages("PerformanceAnalytics")

# Load library
library(quantmod)
library(PerformanceAnalytics)

# Load Data
getSymbols("^GSPC", src="yahoo", from="1991-01-01", to= Sys.Date())
tail(GSPC, 10)


# Visualization only moving averages
graphics.off()

cl_GSPC <- Cl(GSPC)
sma_50 <- SMA(Cl(GSPC), n=50)
sma_200 <- SMA(Cl(GSPC), n=200)

plot(index(sma_50), coredata(sma_50), type = "l", col = "blue",
     main = "50-day and 200-day Moving Averages",
     xlab = "Year", ylab = "Value",
     xaxt = 'n')  # 기본 x축 레이블 제거
lines(index(sma_200), coredata(sma_200), col = "red")
lines(index(GSPC), coredata(cl_GSPC), col = "gray")


# Find crossovers
crossovers <- which(diff(sign(sma_50 - sma_200)) != 0)
cross_dates <- index(sma_50)[crossovers]


# plot crossover points
points(cross_dates, sma_50[crossovers], col="green", pch=19, cex=0.6)


# Add label 'year'
years <- format(index(GSPC), "%Y")
unique_years <- unique(years)

yearly_first_indexes <- sapply(unique_years, function(y) {
  min(which(years == y))
})

axis(1, 
     at=index(GSPC)[yearly_first_indexes], 
     labels=unique_years, 
     las=2, 
     cex.axis=0.7, 
     tick=TRUE)

# Add legend
legend("topleft", 
       legend=c("50-day SMA", "200-day SMA", "SNP500"), 
       col=c("blue", "red", "gray"), 
       lty=1)

