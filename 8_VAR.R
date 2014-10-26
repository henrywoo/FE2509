library("MTS")
da=read.table("q-gdp-ukcaus.txt",header=T)
gdp=log(da[,3:5])
z=gdp[2:126,]-gdp[1:125,]
z=z*100
m1=VAR(z,2)