library(mts)

data(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets)

logR = diff(log(EuStockMarkets))
plot(logR)

#################################################
index.names = dimnames(logR)[[2]]
par(mfrow=c(2,2))
for(i in 1:4)
{
  qqnorm(logR[,i],datax=T,main=index.names[i])
  qqline(logR[,i],datax=T)
  print(shapiro.test(logR[,i]))
}

#################################################
n=dim(logR)[1]
q.grid = (1:n)/(n+1)#?
df=c(1,4,6,10,20,30)
for(i in 1:4)
{
  #windows()
  par(mfrow=c(3,2))
  for(j in 1:6)
  {
    qqplot(logR[,i], qt(q.grid,df=df[j]),
           main=paste(index.names[i], ", df=", df[j]) )
    abline(lm(qt(c(.25,.75),df=df[j])~quantile(logR[,i],c(.25,.75))))
  }
}