setwd("c:/Ying/teaching/MFE Econometrics/AY201415-SEM1/code/")

da=read.table("q-gdpunemp.txt",header=T)  ## Load data
head(da)
dim(da)

zt=cbind(log(da$gdp),da$rate)  ## Create time series
tdx=da[,1]+da[,2]/12  ## Create calendar time
install.packages("MTS")  ## Load MTS package
library("MTS")
colnames(zt) <- c("ln(GDP)","Unrate")
MTSplot(zt,tdx) ## Obtain Figure 1.1 of the textbook
dzt=diffM(zt)   ## Take first difference of each time series
colnames(dzt) <- c("GDP growth","Change rate")
MTSplot(dzt,tdx[2:256]) ## Obtain Figure 1.2 of the textbook
plot(dzt[,1],dzt[,2],xlab="GDP growth",ylab="Change in rate") ## Obtain Figure 1.3 of the textbook

rate=read.table("m-unemp-states.txt",header=T) ## Load states unemployment rates
dim(rate)
head(rate)
tdx=c(1:429)/12+1976
ym=max(rate)  ## maximum unemployment rate, for scaling the plots.
### The following commands create Figure 1.5 of the textbook.
plot(tdx,rate[,1],xlab='Year',ylab='Rate',ylim=c(0,ym+1),type='l')
for(i in 2:50){
lines(tdx,rate[,i],lty=i)
}

