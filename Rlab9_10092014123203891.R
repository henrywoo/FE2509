#install.packages("urca")

#library(urca)
#setwd("c:/Ying/teaching/MFE Econometrics/AY201415-SEM1/code/")

library(urca)
library(fEcofin)
midcapD.ts=read.csv("8_midcapD.ts.csv",header=T,sep=";")
x = midcapD.ts[,2:11]
prices= exp(apply(x,2,cumsum))
options(digits=3)
summary(ca.jo(prices))


library(urca)
mk.maturity=read.csv("8_mk.maturity.csv",header=T)
mk.maturity[2:11,]
mk.zero2=read.csv("8_mk.zero2.csv",header=T,sep=";")
res = ca.jo(mk.zero2[,2:11])
summary(res)
plotres(res)


