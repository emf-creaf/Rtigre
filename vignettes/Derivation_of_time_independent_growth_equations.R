## ----fig.align="center", width=8----------------------------------------------
t <- seq(0, 100, by = .1)
max_y <- 120
k <- .1
B <- -2
dat <- data.frame(t = t,k = k, offset = B, max_y = max_y)
plot(t,Rtigre::td_size(dat, curve_type = "logistic"), type = "l", lwd = 2, ylim = c(0, max_y), col = 1,
     xlab="Time",ylab="Size",main="Age/time-dependent size")
points(t, Rtigre::td_size(dat, curve_type = "schumacher"), type = "l", lwd = 2, col = 2)
points(t, Rtigre::td_size(dat, curve_type = "gompertz"), type = "l",lwd = 2,col = 3)
points(t, Rtigre::td_size(dat, curve_type = "monomolecular"), type = "l",lwd=2,col = 4)
y <- Rtigre::td_size(dat, curve_type = "monomolecular2")
y[t<20] <- 0
points(t, y, type = "l",lwd=2,col = 5)
points(t, Rtigre::td_size(dat, curve_type = "arctangent"), type = "l", lwd = 2, col = 6)
points(t, Rtigre::td_size(dat, curve_type = "rational"), type = "l", lwd = 2, col = 7)
points(t, Rtigre::td_size(dat, curve_type = "arctangent_exp"), type = "l", lwd = 2, col = 8)
legend("bottomright", lty = 1, c("logistic", "schumacher", "gompertz", "monomolecular",
                                 "monomolecular2", "arctangent", "rational", "arctangent exponential"), lwd = 2, cex = 1.1, col = 1:8)

## ----fig.align="center", width=8----------------------------------------------
tdiff <- 1
max_y <- 120
y1 <- seq(.1, 110, length = 1000)
k <- .1
dat <- data.frame(tdiff = tdiff, max_y = max_y, k = k, y1  = y1)
plot(y1, Rtigre::ti_size(dat)-dat$y1, xlab = "Size at t1",
     ylab = "Size at t2 - size at t1", type = "l", lwd = 2, col = 1, ylim = c(0, 45))
points(y1,Rtigre::ti_size(dat, "schumacher")-dat$y1, type = "l", lwd = 2, col = 2)
points(y1,Rtigre::ti_size(dat, "gompertz")-dat$y1, type = "l", lwd = 2, col = 3)
points(y1,Rtigre::ti_size(dat, "monomolecular")-dat$y1, type = "l", lwd = 2, col = 4)
points(y1,Rtigre::ti_size(dat, "monomolecular2")-dat$y1, type = "l", lwd = 2, col = 4)
points(y1,Rtigre::ti_size(dat, "arctangent")-dat$y1, type = "l", lwd = 2, col = 6)
points(y1,Rtigre::ti_size(dat, "rational")-dat$y1, type = "l", lwd = 2, col = 7)
points(y1,Rtigre::ti_size(dat, "arctangent_exp")-dat$y1, type = "l", lwd = 2, col = 8)
legend("topright", lty = 1, c("logistic", "schumacher", "gompertz", "monomolecular",
                          "monomolecular2", "arctangent", "rational", "arctangent exponential"), lwd = 2, cex = 1.1, col = 1:8)

