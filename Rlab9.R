yieldDat = read.table("Rlab9_yields.txt",header=T)
maturity = c((0:5),5.5,6.5,7.5,8.5,9.5)
pairs(yieldDat)
par(mfrow=c(4,3))
for (i in 0:11)
{
  plot(maturity,yieldDat[100*i+1,],type="b")
}

eig = eigen(cov(yieldDat))
eig$values
eig$vectors
par(mfrow=c(1,1))
barplot(eig$values)

par(mfrow=c(2,2))
plot(eig$vector[,1],ylim=c(-.7,.7),type="b")
abline(h=0)
plot(eig$vector[,2],ylim=c(-.7,.7),type="b")
abline(h=0)
plot(eig$vector[,3],ylim=c(-.7,.7),type="b")
abline(h=0)
plot(eig$vector[,4],ylim=c(-.7,.7),type="b")
abline(h=0)

install.packages("tseries")
library("tseries")
adf.test(yieldDat[,1])

n=dim(yieldDat)[1]
delta_yield = yieldDat[-1,] - yieldDat[-n,]

pca_del = princomp(delta_yield)
names(pca_del)
summary(pca_del)
plot(pca_del)

# Uses daily data 2004-2005
stocks = read.csv("Rlab9_Stock_FX_Bond_2004_to_2005.csv",header=T)
attach(stocks)
stocks_subset=as.data.frame(cbind(GM_AC,F_AC,UTX_AC,MRK_AC))
stocks_diff = as.data.frame(100*apply(log(stocks_subset),
                                      2,diff) - FF_data$RF)
names(stocks_diff) = c("GM","Ford","UTX","Merck")
FF_data = read.table("Rlab9_FamaFrenchDaily.txt",header=T)
FF_data = FF_data[-1,] # delete first row since stocks_diff
# lost a row due to differencing
fit1 = lm(as.matrix(stocks_diff)~FF_data$Mkt.RF)
summary(fit1)

fit2 = lm(as.matrix(stocks_diff)~FF_data$Mkt.RF +
            FF_data$SMB + FF_data$HML)
summary(fit2)

dat = read.csv("Rlab9_Stock_FX_Bond.csv")
stocks_ac = dat[,c(3,5,7,9,11,13,15,17)]
n = length(stocks_ac[,1])
stocks_returns = log(stocks_ac[-1,] / stocks_ac[-n,])
fact = factanal(stocks_returns,factors=2,,rotation="none")
print(fact)

loadings = matrix(as.numeric(loadings(fact)),ncol=2)
unique = as.numeric(fact$unique)
