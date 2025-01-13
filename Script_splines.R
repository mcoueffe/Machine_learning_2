# ------------------------------ SPLINES

library(splines)
xx=seq(0,1,length=100)
Bpoly <- matrix(c(xx,xx^2,xx^3),byrow=F,nrow=100)
matplot(Bpoly,type="l")