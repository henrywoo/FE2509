install.packages("fUnitRoots")
library("fUnitRoots")
library("urca")
library("MTS")
setwd("c:/Ying/teaching/MFE Econometrics/AY201415-SEM1/code/")

da=read.table("m-bnd.txt")
head(da)
tail(da)

bnd=da[,4:5]
colnames(bnd) <- c("Aaa","Baa")

par(mfrow=c(2,1)) 
pacf(bnd[,1])
pacf(bnd[,2])
adfTest(bnd[,1],lags=3,type="c")
adfTest(bnd[,2],lags=2,type="c")

m1=VARorder(bnd)

m2=ca.jo(bnd,K=2,ecdet=c("none"))
summary(m2)


wt=bnd[,1]-0.886*bnd[,2]
adfTest(wt,lags=3,type="c")

############### Estimation of ECM model
m1=ECMvar1(bnd,3,wt) ## Given the co-integrated vector
m2=refECMvar1(m1)  ####### Refine the model fit
