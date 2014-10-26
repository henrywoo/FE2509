setwd("c:/Ying/teaching/MFE Econometrics/AY201415-SEM1/code/")
library("MTS")
da=read.table("q-gdp-ukcaus.txt",header=T)
gdp=log(da[,3:5])
dim(gdp)
z=gdp[2:126,]-gdp[1:125,] ## Growth rate
z=z*100 ## Percentage growth rates
dim(z)
MTSplot(z)

ccm(z)

z1=z/100 ### Original growth rates
m0=VARorder(z1)
names(m0)
m0$Mstat

m1=VAR(z1,2) 
m2=refVAR(m1,thres=1.96)



# resi=m1$residuals ### Obtain the residuals of VAR(2) fit.

MTSdiag(m1,adj=12)

VARpred(m1,8)
colMeans(z1) 
sqrt(apply(z1,2,var))
