library(Fgmutils)
x <- X1$LoanPaymentsOverdue
y <- X1$PriceChange
plot(x,y)
n <- length(x)
xm <- mean(x)
ym <- mean(y)
SXX <- sum((x-xm)^2)
SXY <- sum((x-xm)*(y-ym))
b1 <- SXY/SXX
b0 <- ym - xm*b1
q <- qt(0.975,n-2)
r <- (y)- (b1*x) - (b0)
RSS <- sum(r*r)
S <- sqrt(RSS/(n-2))
se <- S/sqrt(SXX)
print (b1)
print (b1-q*se)
print (b1+q*se)
se1 <- sqrt((1/n) + (((4-xm)^2)/(SXX)))
print (b0 + b1*4)
print (b0 + b1*4 - se1*S*q)
print (b0 + b1*4 + se1*S*q)