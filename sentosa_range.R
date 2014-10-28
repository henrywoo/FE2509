library("RMySQL")

con = dbConnect(MySQL(), user="root", password="wfh168178",
                dbname="equities", host="192.168.254.1")

getDF=function(con){
  stocks=c('ATHM','BITA','FXI','SPY','HMIN','HTHT')
  df =NA #<- data.frame()
  for (s in stocks){
    cmd = sprintf("SELECT dt,h-l AS %s FROM bar1d WHERE s='%s' AND dt>'2014-01-24' AND dt<='2014-10-24' ORDER BY dt",s,s)
    print(cmd)
    rs  = dbSendQuery(con, cmd)
    tmp = fetch(rs, n = -1)
    if(is.data.frame(df)){
      df = merge(df,tmp)
    }else{
      df = tmp
    }
  }
  rownames(df) = df$dt
  df=df[,!(names(df) %in% c('dt'))]
  
  #cm=colMeans(df)
  #for (s in stocks){
  #  df[s] = df[s]-cm[s]
  #}
  
  return(df)
}


debug(getDF)
df=getDF(con)
undebug(getDF)

# multiple linear regression
fit1=lm(df$ATHM ~ df$BITA+df$FXI+df$SPY+df$HMIN+df$HTHT)
summary(fit1)
confint(fit1)
###########################################################