r <- runif(1000000)
p <- qpois(r,1)
print (p)
print (mean(p))
