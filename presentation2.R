#MFE 5209 Presentation 1 - Q2-4
rm(list=ls())

if (!"fEcofin" %in% rownames(installed.packages())){
  install.packages("fEcofin", repos="http://R-Forge.R-project.org")
}
if (!"urca" %in% rownames(installed.packages())){
  install.packages("urca", repos="http://R-Forge.R-project.org")
}

library(fEcofin)
library(urca)
library(MTS)
library(fUnitRoots)
#mk.maturity=read.csv("8_mk.maturity.csv",header=T)
mk.maturity[2:11,] # maturity
#mk.zero2=read.csv("8_mk.zero2.csv",header=T,sep=";")#yield
zos = mk.zero2[,2:11]
zos.vecm = ca.jo(zos)
summary(zos.vecm)
#plotres(zos.vecm)

##> VARorder(zos)
##Error in solve.default(xpx, xpy) : 
##  system is computationally singular: reciprocal condition number = 6.42168e-21
for (i in seq(dim(zos)[2])){
  print(ar(zos[,1],method="mle")$order)
}

#Question 4
###############################################################################
# Pair Trading
###############################################################################
#beta_=zos.vecm@Vorg #original matrix of Beta(cointegration vector)
beta=zos.vecm@V # after normalization
#zos.vecm@test.name

# meaningful cointegration vector
mbeta=beta[,1:4] # 10x4

# the 4 stationary series(compuonded yield) we constructed with the cointegration vector
cyield=as.matrix(zos) %*% mbeta # (AB)' = (B'*A') - cyield should be a stationary time series according to VECM

#MTSplot(cyield)

# draw cyield and range lines
plot(cyield[,1],type="o",ylim=c(-0.005,0.005))
lines(cyield[,2],type="o",col='red',ylim=c(-0.005,0.005))
lines(cyield[,3],type="o",col='green',ylim=c(-0.005,0.005))
lines(cyield[,4],type="l",col='blue',ylim=c(-0.005,0.005))

abline(h = c(-0.0005,0.0005, -0.001,0.001, -0.002,0.002)) # spread line

adf1=adfTest(cyield[,1])
adf1@test$p.value
adf2=adfTest(cyield[,2])
adf2@test$p.value
adf3=adfTest(cyield[,3])
adf3@test$p.value
adf4=adfTest(cyield[,4])
adf4@test$p.value