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

legend("bottomright",lty=1,c("logistic","schumacher","gompertz","monomolecular",
"arctangent"),lwd=2,cex=1.1,col=1:6)

