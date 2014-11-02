library("RMySQL")
library(MTS)

con = dbConnect(MySQL(), user="root", password="wfh168178",
                dbname="equities", host="192.168.254.1")

stocks=c('SINA','BIDU','NTES','VIPS','QIHU','CTRP','ATHM','YY',
         'WB','SFUN','CCIH','SOHU','YY','FENG','BITA','RENN',
         'WUBA','YOKU','DANG','JRJC','CYOU','CTRP','JOBS',
         'QUNR','LONG','PWRD','NCTY','DATE','NQ','GOMO')

getDF=function(con, stocks, detrended=TRUE){
  df =NA #<- data.frame()
  for (s in stocks){
    cmd = sprintf("SELECT dt,adjclose AS `%s` FROM dprice WHERE s='%s' 
                  AND dt>'2014-03-01' AND dt<='2014-10-31' ORDER BY dt",s,s)
    print(cmd)
    rs  = dbSendQuery(con, cmd)
    tmp = fetch(rs, n = -1)
    print(dim(tmp))
    if(is.data.frame(df)){
      df = merge(df,tmp)
    }else{
      df = tmp
    }
  }
  rownames(df) = df$dt
  df=df[,!(names(df) %in% c('dt'))]
  
  if(detrended){
    cm=colMeans(df)
    for (s in stocks){
      df[s] = df[s]-cm[s]
    }  
  }
  return(df)
}

vardf=VAR(df)
plot(density(vardf$residuals))
normalTest(vardf$residuals,method='jb')# normality is rejected!

#debug(getDF)
df=getDF(con, stocks)
chinaconcept.cov=cov(df)
chinaconcept.cor=cor(df)
pcor=princomp(covmat = chinaconcept.cor)
pcov=princomp(covmat = chinaconcept.cov)


#undebug(getDF)

# multiple linear regression
fit1=lm(df$ATHM ~ df$BITA+df$FXI+df$SPY+df$HMIN+df$HTHT)
summary(fit1)
confint(fit1)
###########################################################