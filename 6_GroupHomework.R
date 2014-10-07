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
