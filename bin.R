r <- runif(1000000)
p <- qbinom(r,100,0.5)
print (p)
print (mean(p))
