library(Fgmutils)
data <- read.csv("https://gattonweb.uky.edu/sheather/book/docs/datasets/playbill.csv")
x <- data$LastWeek
y <- data$CurrentWeek
xm <- mean(x)
ym <- mean(y)
SXY <- sum((x-xm)*(y-ym))
SXX <- sum((x-xm)*(x-xm))
b1 <- SXY/SXX
b0 <- ym-(b1*xm)
v=seq(0,1300000,10000)
t=v-xm
f=t^2
h1 = qt(0.975, df=16)
h2 = sum((y-b0-(x*b1))^2)
S = sqrt(h2/16)
h3 = sqrt((1/18)+(f/SXX))
h = h1*S*h3
plot(data$LastWeek,data$CurrentWeek)
y1 <- b0 + b1*(v)+h
y2 <- b0 + b1*(v)-h
y3 <- b0 + b1*(v)
y4 <- b0 + v
plot(x,y)
lines(v,y1,type="l",col="blue")
lines(v,y2,type="l",col="red")
lines(v,y3,type="l")
points(450000,400000)
l2 <- sqrt((1/18)+(((xm)^2)/SXX))
l1 <- (b0-10000)/l2
l3 <- l1/S
l4 <- 2*(pt(-l3, 16, lower.tail = FALSE))
print (l4)
plot(x,y)
lines(v,y1,type="l",col="blue")
lines(v,y2,type="l",col="red")
lines(v,y4,type="l")