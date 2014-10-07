#The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. 
#The mean of exponential distribution is 1/lambda and the standard deviation is also also 1/lambda. 
#Set lambda = 0.2 for all of the simulations.
#In this simulation, you will investigate the distribution of averages of 40 exponential(0.2)s.
#Note that you will need to do a thousand or so simulated averages of 40 exponentials.

#Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponential(0.2)s.  You should
#1. Show where the distribution is centered at and compare it to the theoretical center of the distribution.
#2. Show how variable it is and compare it to the theoretical variance of the distribution.
#3. Show that the distribution is approximately normal.
#4. Evaluate the coverage of the confidence interval for 1/lambda: X¯±1.96Sn√.


library(manipulate)
library(ggplot2)

k <- 1000
xvals <- seq(-5, 5, length = k)

myplot <- function(df){
  d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
                  x = xvals,
                  dist = factor(rep(c("Normal", "T"), c(k,k))))
  g <- ggplot(d, aes(x = x, y = y)) 
  g <- g + geom_line(size = 2, aes(colour = dist))
  g
}


manipulate(myplot(mu), mu = slider(1, 20, step = 1))
