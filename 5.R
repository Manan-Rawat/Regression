library(Fgmutils)
data <- read.delim("https://gattonweb.uky.edu/sheather/book/docs/datasets/bonds.txt")
x <- data$CouponRate
y <- data$BidPrice
xm <- mean(x)
ym <- mean(y)
SXY <- sum((x-xm)*(y-ym))
SXX <- sum((x-xm)*(x-xm))
b1 <- SXY/SXX
b0 <- ym-(b1*xm)
v <- seq(2,15,0.001)
y1 <- v*b1 + b0
plot(x,y)
lines(v,y1,type="l")
z <- qt(0.975,33)
e <- ( y - b1*x -b0 )
S <- sum(e*e)/(33)
se <- sqrt(S)/sqrt(SXX)
l <- (1/35) + ((x-xm)^2)/(sum((x-xm)^2))
print(b1-z*se)
print(b1+z*se)
print (4/35)
print (l)