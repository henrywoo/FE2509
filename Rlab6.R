install.packages("Ecdat")
install.packages("tseries")
install.packages("fGarch")
data(Tbrate,package="Ecdat")
library(tseries)
library(fGarch)
# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
Tbill = Tbrate[,1]
Del.Tbill = diff(Tbill)

garch.model.Tbill = garchFit(formula= ~arma(1,0) + garch(1,0),Tbill)
summary(garch.model.Tbill)
garch.model.Tbill@fit$matcoef

res = residuals(garch.model.Tbill)
res_std = res / garch.model.Tbill@sigma.t
par(mfrow=c(2,3))
plot(res)
acf(res)
acf(res^2)
plot(res_std)
acf(res_std)
acf(res_std^2)
