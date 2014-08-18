problem1 = function(){
  
  niter = 1e5 # number of iterations
  below = rep(0,niter) # set up storage
  set.seed(2009)
  for (i in 1:niter)
  {
    # generate random numbers
    r = rnorm(45, mean=.05/253, sd=.23/sqrt(253))
    
    logPrice = log(1e6) + cumsum(r)
    
    # minimum price over next 45 days
    minlogP = min(logPrice)

    below[i] = as.numeric(minlogP < log(950000))
  }
  
  # = 0.50988
  paste(sum(below)/niter)
}

#debug(problem1)
#undebug(problem1)
problem1()