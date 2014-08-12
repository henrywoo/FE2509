niter = 1e5 # number of iterations
below = rep(0,niter) # set up storage
set.seed(2009)
for (i in 1:niter)
{
  r = rnorm(45,mean=.05/253,
            sd=.23/sqrt(253)) # generate random numbers
  logPrice = log(1e6) + cumsum(r)
  minlogP = min(logPrice) # minimum price over next 45 days
  below[i] = as.numeric(minlogP < log(950000))
}