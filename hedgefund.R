##
## FE5209 Problem 1
##
problem1 = function(seed_){
  niter = 1e5 # number of iterations
  below = rep(0,niter) # set up storage
  set.seed(seed_)
  for (i in 1:niter)
  {
    # generate random numbers with normal distribution
    r = rnorm(45, mean=.05/253, sd=.23/sqrt(253))
    
    logPrice = log(1e6) + cumsum(r)
    
    # minimum price over next 45 days
    minlogP = min(logPrice)

    below[i] = as.numeric(minlogP < log(950000))
  }
  
  return(sum(below)/niter)
}

#debug(problem1)
#undebug(problem1)
j=0
for (i in 2001:2010){
  j = j + problem1(i)  
}
paste("the probability that the value of the stock will be",
      "below $950,000 at the close of at least one of the",
      "next 45 trading days is", j/10)
