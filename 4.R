library(Fgmutils)
anscombe <- read.csv("D:/Downloads/anscombe.csv")
xx1<-anscombe$x1
xx2<-anscombe$x2
xx3<-anscombe$x3
xx4<-anscombe$x4
yy1<-anscombe$y1
yy2<-anscombe$y2
yy3<-anscombe$y3
yy4<-anscombe$y4
sxy1 <- sum((xx1-mean(xx1))*(yy1-mean(yy1)))
sxy2 <- sum((xx2-mean(xx2))*(yy2-mean(yy2)))
sxy3 <- sum((xx3-mean(xx3))*(yy3-mean(yy3)))
sxy4 <- sum((xx4-mean(xx4))*(yy4-mean(yy4)))
syy1 <- sum((xx1-mean(xx1))*(xx1-mean(xx1)))
syy2 <- sum((xx2-mean(xx2))*(xx2-mean(xx2)))
syy3 <- sum((xx3-mean(xx3))*(xx3-mean(xx3)))
syy4 <- sum((xx4-mean(xx4))*(xx4-mean(xx4)))
be1<-sxy1/syy1
be2<-sxy2/syy2
be3<-sxy3/syy3
be4<-sxy4/syy4
bo1<-mean(yy1)-(be1*mean(xx1))
bo2<-mean(yy2)-(be2*mean(xx2))
bo3<-mean(yy3)-(be3*mean(xx3))
bo4<-mean(yy4)-(be4*mean(xx4))
v <- seq(0,20,0.001)
y1 <- v*be1 + bo1
y2 <- v*be2 + bo2
y3 <- v*be3 + bo3
y4 <- v*be4 + bo4

plot(xx1,yy1)
lines(v,y1,type="l")

plot(xx2,yy2)
lines(v,y2,type="l")

plot(xx3,yy3)
lines(v,y3,type="l")

plot(xx4,yy4)
lines(v,y4,type="l")

