test_that("Linear fit of rate", {

   # In the examples below we will calculate a time-dependent growth curve and will
   # recover the input k-value.

   # Common parameters. Simple example.
   tdiff <- 5
   t <- seq(1, 100, by = tdiff)
   max_y <- 120
   k <- .05

   dat <- data.frame(t = t, max_y = max_y, offset = 2, k = k)
   y <- td_size(dat, "logistic") + rnorm(length(t))*.01
   plot(t, y, xlab = "Time", ylab = "Size")

   dat <- data.frame(tdiff = tdiff, max_y = max_y, y1 = y[-length(t)], y2 = y[-1])
   r1 <- fit_rate(dat, ~1, curve_type = "logistic")

   ci <- confint(r1)
   expect_true(coef(r1) > ci[1] & coef(r1) < ci[2])

   ## Fake climatic data.
   temp <- runif(100,18.6,21.3)
   prec <- runif(100,359,514)
   t <- c(10,20)
   intercept <- .02
   coef_temp <- .00061
   coef_prec <- .000052
   k <- intercept+coef_temp*temp+coef_prec*prec+rnorm(length(temp))*.001
   y1 <- max_y/(1+exp(-(k*t[1]-2)))
   y2 <- max_y/(1+exp(-(k*t[2]-2)))
   dat <- data.frame(tdiff=t[2]-t[1],max_y=max_y,y1=y1,y2=y2,temp=temp,prec=prec)


})
