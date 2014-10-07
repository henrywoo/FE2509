install.packages("Ecdat")
library(Ecdat)
data(CRSPday)
crsp=CRSPday[,7]
acf(crsp)
acf(as.numeric(crsp))

arima(crsp,order=c(1,0,0))
arima(crsp,order=c(2,0,0))

