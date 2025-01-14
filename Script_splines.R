# ------------------------------ SPLINES

library(splines)
xx=seq(0,1,length=100)
Bpoly <- matrix(c(xx,xx^2,xx^3),byrow=F,nrow=100)
matplot(Bpoly,type="l")

BScub <-bs(xx,degre=3,knots=c(0.3,0.8))
matplot(BScub,type="l")

BSlin <-bs(xx,degre=1,knots=c(0.3,0.8))
matplot(BSlin,type="l")

n <- 100
set.seed(546) 
x <- sort(runif(n))
f <- sin(2*pi*x)
sigma2 <- 0.1
erreur <- rnorm(n,0,sqrt(sigma2))
y <- f+erreur
plot(x,y)
lines(x,f)

BScub <-bs(x,degre=3,knots=c(0.3,0.8))
dfs3 <- data.frame(BScub,Y=y)
regspl3 <- lm(Y~.,data=dfs3)
summary(regspl3)

plot(x,y)
lines(x,regspl3$fit,col=2,lwd=2)
abline(v=c(0.3,0.8),lty=2)

BSlin <-bs(x,degre=1,knots=c(0.2,0.5),intercept = FALSE,
           Boundary.knots =c(-.1,1.1))
dfs1 <- data.frame(BSlin,Y=y)
regspl1 <- lm(Y~.,data=dfs1)
summary(regspl1)

plot(x,y)
lines(x,regspl1$fit,col=2,lwd=2)
abline(v=c(0.2,0.5),lty=2)

splL <- smooth.spline(x,y)
names(splL)
splL 


ozone <- read.table("ozone.txt",header=T,sep=";",na.strings=".")
ozone <- na.omit(ozone)
ozone$date=NULL
dfSimple <- ozone
Xozone <- model.matrix(O3~.,data=ozone)[,-1]
dfpoly <- data.frame(O3=ozone$O3,Xozone,Xozone^2,Xozone^3)
dfinter <- data.frame(O3=ozone$O3,model.matrix(O3~.^2,data=ozone)[,-1])
###pour les splines
library(splines)
BB <- NULL
for(i in 1:ncol(Xozone)){
  var <- Xozone[,i]
  BX <- bs(var,knots=quantile(var,prob=c(.25,.5,.75)),degre=3,
           Boundary.knots=c(min(var),max(var)))
  colnames(BX) <- paste(colnames(Xozone)[i],"-b",1:6,sep="")
  BB <- cbind(BB,BX)
}
dfspline <- data.frame(O3=ozone$O3,BB)
plot(x,y)
lines(splL$x,splL$y,col=2)
splL0 <- smooth.spline(x,y,lambda=0)
lines(splL0$x,splL0$y,col=3)
splL0

splL1000 <- smooth.spline(x,y,lambda=1000)
lines(splL1000$x,splL1000$y,col=4)

splL1000
