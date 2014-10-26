#  Figure 9.8

data(Mishkin,package="Ecdat")

x= as.vector(Mishkin[,1])  # pai1 = one-month inflation rate 
                           #  (in percent, annual rate) 


year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)

fit = arima(x,c(1,0,0))

postscript("inflation_AR1_acf.ps",width=9,height=4.75) # Fig 9.8
par(mfrow=c(1,2))
acf(x,main="Inflation rate")
acf(fit$resid,main="Residuals from AR(1)")
graphics.off()

#  Figure 9.10


logn=log(n)

#####  Fitting AR models
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20)
{
fit = arima(diff(x),order=c(i,0,0))
resultsdiff[i,2] = fit$aic
resultsdiff[i,3] = resultsdiff[i,2] + (logn-2)*i
resultsdiff[i,1]=i
}
postscript("inflation_diff_arfits.ps")         # Fig 9.10
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion",
   cex.lab=1.35,cex.axis=1.35,
   main="AR fits to changes in inflation rate",
   cex.main=1.35,cex=2,pch="*",ylim=c(2440,2560))
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2)
legend(12,2565,c("AIC","BIC"),pch=c("*","o"),cex=2,box.lty=0)
graphics.off()

