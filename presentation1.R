#MFE 5209 Presentation 1
rm(list=ls())

if (!"fEcofin" %in% rownames(installed.packages())){
  install.packages("fEcofin", repos="http://R-Forge.R-project.org")
}
if (!"urca" %in% rownames(installed.packages())){
  install.packages("urca", repos="http://R-Forge.R-project.org")
}

library(fEcofin)
library(urca)

# import time series data
if (!exists("midcapD.ts")){
  midcapD.ts=read.csv("8_midcapD.ts.csv",header=T,sep=";")
}

# Columns 1 and 22 contain the date and market returns, respectively.
#head(midcapD.ts)

# the data we are using
data = midcapD.ts[,2:11]

# P_t = P_0 * exp(r_1 + ...+ r_t)
prices= exp(apply(data,2,cumsum))

#controls the number of digits to print when printing numeric values
options(digits=3)
summary(ca.jo(prices))