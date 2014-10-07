install.packages("Ecdat")
data(Tbrate,package="Ecdat")
install.packages("tseries")
library(tseries)
# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
plot(Tbrate)
acf(Tbrate)
adf.test(Tbrate[,1])
adf.test(Tbrate[,2])
adf.test(Tbrate[,3])

diff_rate = diff(Tbrate)
adf.test(diff_rate[,1])
adf.test(diff_rate[,2])
adf.test(diff_rate[,3])
pairs(diff_rate) # scatterplot matrix
plot(diff_rate) # time series plots

par(mfrow=c(1,1))
boxplot(diff_rate[,1] ~ cycle(diff_rate))

install.packages("forecast")
library(forecast)
auto.arima(Tbrate[,1],max.P=0,max.Q=0,ic="aic")
auto.arima(Tbrate[,1],max.P=0,max.Q=0,ic="bic")

fit1 = arima(Tbrate[,1],order=c(?,?,?))
acf(residuals(fit1))
Box.test(residuals(fit1), lag = 10, type="Ljung")

resid2 = residuals(fit1)^2
acf(resid2)
Box.test(resid2, lag = 10, type="Ljung")

data(Tbrate,package="Ecdat")
# r = the 91-day Treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
# fit the nonseasonal ARIMA model found by auto.arima
attach(as.list(Tbrate))
auto.arima(pi,max.P=0,max.Q=0,ic="bic")
fit = arima(pi,order=c(?,?,?))
forecasts = predict(fit,36)
plot(pi,xlim=c(1980,2006),ylim=c(-7,12))
lines(seq(from=1997,by=.25,length=36), forecasts$pred,col="red")
lines(seq(from=1997,by=.25,length=36), forecasts$pred + 1.96*forecasts$se,
   col="blue")
lines(seq(from=1997,by=.25,length=36), forecasts$pred - 1.96*forecasts$se,
   col="blue")












