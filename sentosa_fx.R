library("RMySQL")

con = dbConnect(MySQL(), user="root", password="wfh168178",
                dbname="equities", host="192.168.254.1")

s="EUR.USD"
cmd = sprintf("SELECT dt,o,h,c,l FROM bar15sfx WHERE s='%s' AND dt>'2014-10-28' AND dt<='2014-10-30' ORDER BY dt",s)
print(cmd)
rs  = dbSendQuery(con, cmd)
tmp = fetch(rs, n = -1)
plot(x=tmp$dt, y=tmp$o, type='l')