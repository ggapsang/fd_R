# install.packages("quantmode")
# install.packages("PerformanceAnalytics")

# LOAD LIBRARY
library(quantmod)
library(PerformanceAnalytics)
graphics.off()

# LOAD DATA
getSymbols("SPY", scr="yahoo", from="1993-01-29", to=Sys.Date())
tail(SPY)


# VISUALIZATION

## CACLUATE SMA
cl_SPY <- Cl(SPY)
sma_50 <- SMA(cl_SPY, n=50)
sma_200 <- SMA(cl_SPY, n=200)

plot(index(SPY), coredata(cl_SPY), type='l', col='black',
     main = "Moving Averages - SPY : 50, 200",
     xlab = "Year", ylab="Value",
     xaxt = 'n')

## ADD 'YEAR' LABEL
years <- format(index(SPY), "%Y")
unique_years <- unique(years)

yearly_first_indexes <- sapply(unique_years, function(y) {
  min(which(years == y))
})

axis(1, 
     at=index(SPY)[yearly_first_indexes], 
     labels=unique_years, 
     las=2, 
     cex.axis=0.7, 
     tick=TRUE)

## ADD LEGEND
legend("topleft", 
       legend=c("50-day SMA", "200-day SMA", "SNP500"), 
       col=c("green3", "orange", "black"), 
       lty=1)

## PLOT OTHER LINES
lines(index(sma_50), coredata(sma_50), col = 'green3')
lines(index(sma_200), coredata(sma_200), col = 'orange')


# SELECT CROSSOVERS
crossovers <- which(diff(sign(sma_50 - sma_200)) !=0)
cross_dates <- index(sma_50)[crossovers]

# PLOT CROSSOVER points
# Identify crossover points
golden_cross <- which(diff(sign(sma_50 - sma_200)) == 2) + 1
dead_cross <- which(diff(sign(sma_50 - sma_200)) == -2) + 1

# Plot Golden cross
if (length(golden_cross) > 0) {
  points(index(sma_50)[golden_cross], sma_50[golden_cross], col="red3", pch=19, cex=0.5)
  abline(v=index(sma_50)[golden_cross], col="red3", lty=2)
}

# Plot Dead cross
if (length(dead_cross) > 0) {
  points(index(sma_50)[dead_cross], sma_50[dead_cross], col="blue3", pch=19, cex=0.5)
  abline(v=index(sma_50)[dead_cross], col="blue3", lty=2)
}
