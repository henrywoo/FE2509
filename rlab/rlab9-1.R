library(tseries)
data(Tbrate,package='Ecdat')
plot(Tbrate)
adf.test(Tbrate[,1])
adf.test(Tbrate[,2])
adf.test(Tbrate[,3])


diff_rate = diff(Tbrate)
adf.test(diff_rate[,1])
adf.test(diff_rate[,2])
adf.test(diff_rate[,3])
pairs(diff_rate) # scatterplot matrix
plot(diff_rate) # time series plots

acf(diff_rate) # auto- and cross-correlations

par(mfrow=c(1,1))
boxplot(diff_rate[,1] ~ cycle(diff_rate))

#Problem 4
library(forecast)
auto.arima(Tbrate[,1],max.P=0,max.Q=0,ic="aic")

#Problem 5
fit1 = arima(Tbrate[,1],order=c(16,1,1))
acf(residuals(fit1))
Box.test(residuals(fit1), lag = 10, type="Ljung")

#Problem 6
resid2 = residuals(fit1)^2
acf(resid2)
Box.test(resid2, lag = 10, type="Ljung")