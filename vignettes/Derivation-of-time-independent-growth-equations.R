## ----fig.align="center", width=8----------------------------------------------
t <- seq(1,100,by=.1)
max_y <- 120
k <- .1
offset <- -2
dat <- data.frame(t=t,k=k,offset=offset,max_y=max_y)
plot(t,Rtigre::td_size(dat,curve_type="logistic"),type="l",lwd=2,ylim=c(0,max_y),col=1,
     xlab="Time",ylab="Size",main="Age/time-dependent size")
points(t,Rtigre::td_size(dat,curve_type="schumacher"),type="l",lwd=2,col=2)
points(t,Rtigre::td_size(dat,curve_type="gompertz"),type="l",lwd=2,col=3)
points(t,Rtigre::td_size(dat,curve_type="monomolecular"),type="l",lwd=2,col=4,)
points(t,Rtigre::td_size(dat,curve_type="arctangent"),type="l",lwd=2,col=5)
points(t,Rtigre::td_size(dat,curve_type="hyperbolic"),type="l",lwd=2,col=6)

legend("bottomright",lty=1,c("logistic","schumacher","gompertz","monomolecular",
"arctangent","hyperbolic"),lwd=2,cex=1.1,col=1:6)

## ----fig.align="center", width=8----------------------------------------------
tdiff <- 5
max_y <- 120
y1 <- seq(1,110)
k <- .1
dat <- data.frame(tdiff=tdiff,max_y=max_y,k=k,y1=y1)
plot(y1,Rtigre::ti_size(dat)-dat$y1,xlab="Size at t1",
     ylab="Size at t2 - size at t1",type="l",lwd=2,col=1,ylim=c(0,100))
points(y1,Rtigre::ti_size(dat,"schumacher")-dat$y1,type="l",lwd=2,col=2)
points(y1,Rtigre::ti_size(dat,"gompertz")-dat$y1,type="l",lwd=2,col=3)
points(y1,Rtigre::ti_size(dat,"monomolecular")-dat$y1,type="l",lwd=2,col=4)
points(y1,Rtigre::ti_size(dat,"arctangent")-dat$y1,type="l",lwd=2,col=5)
points(y1,Rtigre::ti_size(dat,"hyperbolic")-dat$y1,type="l",lwd=2,col=6)
legend("topright",lty=1,c("logistic","schumacher","gompertz","monomolecular",
  "arctangent","hyperbolic"),lwd=2,cex=1.1,col=1:6)

