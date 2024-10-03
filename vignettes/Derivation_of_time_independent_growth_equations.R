## ----fig.align="center", width=8----------------------------------------------
t <- seq(0, 200, by = .1) ; k  <- 0.1 ; B <- -2
dat <- data.frame(t = t, k = 0.1, offset = -2, max_y = 120)
df <- data.frame(x = t, y = Rtigre::td_size(dat, curve_type = "logistic"), curve_type = "logistic")
for (i in Rtigre:::all_curve_types()[-c(1, 10)]) {
  y <- Rtigre::td_size(dat, curve_type = i)
  if (any(i %in% c("schumacher", "monomolecular", "monomolecular2"))) y[(k*t+B) < 0] <- 0
  df <- rbind(df, data.frame(x = t, y = y, curve_type = i))
}
print(ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = curve_type)) + ggplot2::geom_line())

## ----fig.align="center", width=8----------------------------------------------
tdiff <- 1
max_y <- 120
y1 <- seq(0, max_y, length = 1000)
k <- .1
dat <- data.frame(tdiff = tdiff, max_y = max_y, k = k, y1  = y1)
plot(y1, Rtigre::ti_size(dat)-dat$y1, xlab = "Size at t1",
     ylab = "Size at t2 - size at t1", type = "l", lwd = 2, col = 1, ylim = c(0, 10))
points(y1,Rtigre::ti_size(dat, "schumacher")-dat$y1, type = "l", lwd = 2, col = 2)
points(y1,Rtigre::ti_size(dat, "gompertz")-dat$y1, type = "l", lwd = 2, col = 3)
points(y1,Rtigre::ti_size(dat, "monomolecular")-dat$y1, type = "l", lwd = 2, col = 4)
points(y1,Rtigre::ti_size(dat, "monomolecular2")-dat$y1, type = "l", lwd = 2, col = 5)
points(y1,Rtigre::ti_size(dat, "arctangent")-dat$y1, type = "l", lwd = 2, col = 6)
points(y1,Rtigre::ti_size(dat, "rational")-dat$y1, type = "l", lwd = 2, col = 7)
points(y1,Rtigre::ti_size(dat, "arctangent_exp")-dat$y1, type = "l", lwd = 2, col = 8)

