
rlab9.2 = function(theorder){
  data(Tbrate,package="Ecdat")
  
  # r = the 91-day Treasury bill rate
  # y = the log of real GDP
  # pi = the inflation rate
  # fit the nonseasonal ARIMA model found by auto.arima
  
  pi=Tbrate[,3]
  
  autofit = auto.arima(pi,max.P=0,max.Q=0,ic="bic")
  
  fit = arima(pi,order=theorder)
  
  forecasts = predict(fit,36)
  
  plot(pi,xlim=c(1980,2006),ylim=c(-7,12))
  
  lines(seq(from=1997,by=.25,length=36),forecasts$pred,col="red")
  lines(seq(from=1997,by=.25,length=36),forecasts$pred + 1.96*forecasts$se,col="blue")
  lines(seq(from=1997,by=.25,length=36),forecasts$pred - 1.96*forecasts$se,col="blue")
}

debug(rlab9.2)
rlab9.2(c(1,1,1))
undebug(rlab9.2)