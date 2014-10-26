library(MTS)
C=matrix(c(0.8,-0.3,0.4,0.6),nrow = 2)
S=matrix(c(2,0.5,0.5,1.0),nrow = 2)
m1 = VARMAsim(300,arlags = c(1),phi = C,sigma = S)