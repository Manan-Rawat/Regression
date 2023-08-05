library(Fgmutils)
data <- read.delim2("https://gattonweb.uky.edu/sheather/book/docs/datasets/confood1.txt")
x <- data$Price
y <- data$Sales
x1 <- as.numeric(x)
xm <- mean(x1)
ym <- mean(y)
SXX <- sum((x1-xm)*(x1-xm))
SXY <- sum((x1-xm)*(y-ym))
b1 <- SXY/SXX
b0 <- ym - xm*b1
v <- seq(0.5,0.9,0.00001)
f <- v*b1+b0
plot(x1,y)
lines(v,f,type="l")

xl <- log(x1)
yl <- log(y)
xml <- mean(xl)
yml <- mean(yl)
SXXl <- sum((xl-xml)*(xl-xml))
SXYl <- sum((xl-xml)*(yl-yml))
b1l <- SXYl/SXXl
b0l <- yml - xml*b1l
vl <- seq(-0.6,-0.1,0.00001)
fl <- vl*b1l + b0l
plot(xl,yl)
lines (vl,fl,type="l")
e <- (yl - xl*b1l - b0l)
S <- sqrt(sum(e^2)/50)
h <- 1/52 + ((xl-xml)^2)/(sum((xl-xml)^2))
r <- e/(S*(sqrt(1-h)))
plot(xl,r)