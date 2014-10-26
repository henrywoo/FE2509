
data(Mishkin,package="Ecdat")
library(forecast)
library("tseries")

x= as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 

par(mfrow=c(2,2))
plot(x,main="Inflation rate")
plot(diff(x),main="Difference")
acf(x,main="Inflation rate")
acf(diff(x),main="Difference")
adf.test(x)
pp.test(x)
kpss.test(x)

dx = diff(x)
adf.test(dx)
pp.test(dx)
kpss.test(dx)


auto.arima(x)

auto.arima(x,d=0,ic="bic",stationary=T)
fitauto0 = arima(x,order=c(2,0,1))
polyroot( c(1,-fitauto0$coef[1:2]) )