setwd("c:/Ying/teaching/MFE Econometrics/AY201415-SEM1/code/")

dat = read.csv("Stock_FX_bond.csv",header=TRUE)
names(dat)
attach(dat) 
par(mfrow=c(1,2)) 
plot(GM_AC) 
plot(F_AC)
n = dim(dat)[1]
GMReturn = GM_AC[2:n]/GM_AC[1:(n-1)] - 1
FReturn = F_AC[2:n]/F_AC[1:(n-1)] - 1 
par(mfrow=c(1,1)) 
plot(GMReturn,FReturn)

GMLogReturn = diff(log(GM_AC))
cor(GMLogReturn,GMReturn)



