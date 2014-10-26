library("MTS")
da=read.table("q-gdp-ukcaus.txt",header=T)
gdp=log(da[,3:5])
dim(gdp)

z=gdp[2:126,]-gdp[1:125,] ## Growth rate
z=z*100 ## Percentage growth rates
dim(z)

Z=z[3:125,]
X=cbind(rep(1,123),z[2:124,],z[1:123,])
X=as.matrix(X)
XPX=t(X)%*%X
XPXinv=solve(XPX)
Z=as.matrix(Z)
XPZ=t(X)%*%Z
bhat=XPXinv%*%XPZ
bhat

A=Z-X%*%bhat
Sig=t(A)%*%A/(125-(3+1)*2-1)
Sig
COV=kronecker(Sig,XPXinv)
se=sqrt(diag(COV))
para=cbind(bhat,se,bhat/se)
para
Sig1=t(A)%*%A/(125-2) ## MLE of Sigma_a
Sig1