#  Example 17.4

DowJones30=read.csv("9_DowJones30.csv",header=T,sep=";")
pcaDJ = prcomp(DowJones30[,2:31])
summary(pcaDJ)