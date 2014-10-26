install.packages("Ecdat") 
install.packages("fGarch") 
library(Ecdat)
library(fGarch)
data(SP500,package="Ecdat")
returnBlMon = SP500$r500[1805]
x = SP500$r500[(1804-2*253+1):1804]
plot(c(x,returnBlMon))
results = garchFit(~arma(1,0)+garch(1,1),data=x,cond.dist="std")
dfhat = as.numeric(results@fit$par[6])
forecast = predict(results,n.ahead=1)

probBlackMonday = pstd(returnBlMon,mean=forecast$meanForecast,sd=forecast$standardDeviation,nu=dfhat)
round(probBlackMonday,7)

std_res =results@residuals/results@sigma.t 
par(mfrow=c(1,3))
plot(std_res) 
acf(std_res) 
acf(std_res^2)

summary(results)

fitAR1 = arima(x,order=c(1,0,0))
fitAR1 
par(mfrow=c(1,3))
residAR1 = residuals(fitAR1)
plot(residAR1) 
acf(residAR1) 
acf(residAR1^2)

