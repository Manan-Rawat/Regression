rm(list = ls())
library(Fgmutils)
data <- read.delim("https://gattonweb.uky.edu/sheather/book/docs/datasets/responsetransformation.txt")
x <- data$x
y <- data$y
plot(x,y)
xm <- mean(x)
ym <- mean(y)
SXX <- sum((x-xm)^2)
SXY <- sum((x-xm)*(y-ym))
b1 <- SXY/SXX
b0 <- ym-(xm*b1)
v <- seq(0,5,0.001)
f <- b0 +b1*v
plot(x,y)
lines(v,f,type="l")

e <- y - (x*b1) - b0
S <- sqrt((sum(e^2))/248)
l <- 1/250 + ((x-xm)^2)/((sum((x-xm)^2)))
r <- e/(S*sqrt(1-l))
plot(x,r)

plot(x,sqrt(abs(r)))
v1 <- seq(0,100,0.01)
f1 <- b0+b1*v1
plot(y,x)

