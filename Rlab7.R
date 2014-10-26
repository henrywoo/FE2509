library("fEcofin")
library(mnormt)
Berndt = berndtinvest[,5:6] 
Y = as.matrix(Berndt)
loglik = function(par)
{
mu = par[1:2]
A= matrix(c(par[3],par[4],0,par[5]),nrow=2,byrow=T)
scale_matrix = t(A)%*%A
df = par[6]
f = -sum(log(dmt(Y, mean=mu,S=scale_matrix,df=df)))
f
} 
A=chol(cov(Y)) 
start=as.vector(c(apply(Y,2,mean),A[1,1],A[1,2],A[2,2],4))
fit_mvt = optim(start,loglik,method="L-BFGS-B",lower=c(-.2,-.2,-.3,-.3,-.3,2.2), upper=c(.2,.2,.3,.3,.3,15),hessian=T)
muhat = fit_mvt$par[1:2]
dfhat = fit_mvt$par[6]
A= matrix(c(fit_mvt$par[3],fit_mvt$par[4],0,fit_mvt$par[5]), nrow=2,byrow=T)
scaleMatrixhat = t(A)%*%A
options(digits=5)
muhat 
dfhat 
scaleMatrixhat


alpha = .05
w =  c ( 0.3, 0.7)
muP = w %*% muhat
scaleP = sqrt(w %*% scaleMatrixhat %*% w)
VaR05 =100000*( -muP + scaleP*qt(1-alpha,dfhat))
ES05 = 100000*(-muP + scaleP* dt(qt(alpha,dfhat),dfhat)/alpha*(dfhat + qt(alpha,dfhat)"2)/(dfhat-1) )?
muP 
scaleP 
VaR05
ES05

n=dim(Y[1])
w= c(.3,.7) 
Bboot = 100
VaR05boot = rep(0,Bboot) 
Dfhatboot = rep(0,Bboot) 
Y = as.matrix(Berndt)
for (iboot in 1:Bboot)
{
Ind = sample(n,replace=T) 
Yboot =Y[ind,]
Loglik = function(par)
{
mu = par[1:2]
A = matrix(c(par[3],par[4],0,par[5]),nrow= 2,byrow = T)
scale_matrix = t(A)%*%A
df = par[6]
f = -sum(log(dmt(Yboot, mean=mu,S=scale_matrix,df=df)))
f
}
A = chol(cov(Yboot))
start = as.vector(c(apply(Yboot,2,mean),A[1,1],A[1,2],A[2,2],4))
fit_mvt = optim(start,loglik,method="L-BFGS-B",lower=c(-.02,-.02,-.1,-.1,-.1,2), upper = c(.02,.02,.1,.1,.1,15),hessian=T)
muhat=fit_mvt$par[1:2]
dfhatboot[iboot]=fit_mvt$par[6] 
A=matrix(c(fit_mvt$par[3],fit_mvt$par[4],0,fit_mvt$par[5]),nrow=2,byrow=T) scaleMatrixhat t(A)%*%A
?
muP=w %*% muhat
scaleP = sqrt(w %*% scaleMatrixhat %*% w) 
VaR05boot[iboot]=100000*(-muP + scaleP*qt(.95,dfhat))
}
quantile(VaR05boot,c(.05,.95)) 
par(mfrow=c(1,2)) 
plot(density(dfhatboot),main="DF") 
plot(density(VaR05boot),main="VaR05")