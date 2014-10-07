setwd("c:/Ying/teaching/MFE Econometrics/DDP/new/code/Rcode/")
dat = read.csv("Rlab3_Stock_FX_Bond_2004_to_2006.csv",header=T)
prices = dat[,c(5,7,9,11,13,15,17,24)]
n = dim(prices)[1]

dat2 = as.matrix(cbind(dat[(2:n),3]/365, 100*(prices[2:n,]/prices[1:(n-1),] - 1)))
names(dat2)[1] = "treasury"
risk_free = dat2[,1]
ExRet = dat2[,2:9] - risk_free
market = ExRet[,8]
stockExRet = ExRet[,1:7]

fit_reg = lm(stockExRet~market)
summary(fit_reg)
res = residuals(fit_reg)
pairs(res)
options(digits=3)
betas=fit_reg$coeff[2,]

betas*mean(market)
apply(stockExRet,2,mean)

res = residuals(fit_reg)
options(digits=3)
cor(res)

4*betas
