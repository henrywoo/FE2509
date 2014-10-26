install.packages("MTS")  ## Load MTS package
library("MTS")

sig=diag(2) % create the 2-by-2 identity matrix
x=rmvnorm(300,rep(0,2),sig) % generate random draws
MTSplot(x) % Obtain time series plots (output not shown)
ccm(x)