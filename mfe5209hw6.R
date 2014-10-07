dat = read.csv("Rlab9_Stock_FX_Bond.csv" ,header=T)
prices = as.matrix(dat[1:500,c(3,5,7,9,11)])

r=diff(prices,1)

# sample mean vector
smv = colMeans(r)

# sample covariance matrix
ecm = var(r)


sharenum=floor(10000000/prices[500,])

S=50000000

w=matrix(rep(0.2,5))
m=smv %*% w
sigma=sqrt(t(w) %*% ecm %*% w)
var1day=-S*qnorm(0.1, mean = m,sd = sigma)
# -S*(m+qnorm(0.1) *sigma) # the same result
var5day=-S*qnorm(0.1, mean = 5*m,sd = sqrt(5)*sigma)