library(Fgmutils)
x <- Book1$RunSize
y <- Book1$RunTime
xm <- mean(x)
ym <- mean(y)
SXY <- sum((x-xm)*(y-ym))
SXX <- sum((x-xm)*(x-xm))
b1 <- SXY/SXX
b0 <- ym-(b1*xm)
v=seq(50,350,0.01)
y1 <- v*b1 + b0
plot(x,y)
lines(v,y1,type="l")

z <- qt(0.975,18)

e <- ( y - b1*x -b0 )
S <- sum(e*e)/(18)

se <- sqrt(S)/sqrt(SXX)

l <- (1/20)+((x-xm)^2)/(sum((x-xm)^2))
print(b1-z*se)
print(b1+z*se)
sr <- e/(sqrt(S*(1-l)))

plot(y,e)
qqnorm(sr)
qqline(sr,col='blue')
plot(b0+b1*x,sqrt(abs(sr)))

plot(l,sr)