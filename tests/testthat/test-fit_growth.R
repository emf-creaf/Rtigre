test_that("Testing fit_growth", {

#
  # source("./R/fit_growth.R")
  # source("./R/helpers.R")
  # source("./R/fit_rate.R")
  # source("./R/rate_gr.R")
  # source("./R/eval_gr.R")
  # source("./R/string_gr.R")
  # source("./R/gr_logistic.R")
  # source("./R/fit_optim.R")


  ## Common parameters. Simple example.
  tdiff <- 5
  t <- seq(1, 100, by = tdiff)
  max_y <- 120
  k <- .1

  ## Fake climatic data.
  temp <- runif(1000, 18.6, 21.3)
  prec <- runif(1000, 359, 514)
  t <- c(10, 20)
  cc <- c(intercept = .02, coef_temp = .00061, coef_prec = .000052)
  k <- cc[1] + cc[2]*temp + cc[3]*prec + rnorm(length(temp))*.001
  y1 <- max_y/(1+exp(-(k*t[1]-2)))
  y2 <- max_y/(1+exp(-(k*t[2]-2)))
  dat <- data.frame(tdiff = t[2] - t[1], max_y = max_y, y1 = y1, y2 = y2, temp = temp, prec = prec)
  dat$intercept <- rep(1, nrow(dat))
  fo <- ~ intercept + temp + prec

  # Recover the coefficients.
  r1 <- dat |> fit_growth(fo, log_transf = F, positive_rate = FALSE, verbose = F)
  coef_r1 <- coef(r1)
  expect_lt(max(abs(1-coef_r1/cc))*100, 20)
  expect_gt(cor(dat$y2-dat$y1, predict(r1)), .9)


  # positive_rate = TRUE changes very little in this example.
  r2 <- dat |> fit_growth(fo, log_transf = F, positive_rate = TRUE, verbose = F)
  expect_gt(cor(dat$y2-dat$y1, predict(r2)), .9)
  expect_gt(cor(predict(r1), predict(r2)), .99)


  # Check that everything is ok.
  expect_s3_class(dat |> fit_growth(fo, log_transf = F, positive_rate = FALSE, verbose = F), "nls")
  expect_s3_class(dat |> fit_growth(fo, log_transf = T, positive_rate = FALSE, verbose = F), "nls")
  expect_s3_class(dat |> fit_growth(fo, log_transf = F, positive_rate = FALSE, verbose = F), "nls")
  expect_s3_class(dat |> fit_growth(fo, log_transf = T, positive_rate = FALSE, verbose = F), "nls")


  # Make predicted increment negative.
  dat2 <- dat
  dat2$prec <- -dat$prec
  dat2$temp <- -dat$temp
  pred1_y <- predict(r1, newdata = dat2)
  expect_true(all(pred1_y < 0))

  # But with positive_rate = TRUE this does not happen.
  pred2_y <- predict(r2, newdata = dat2)
  expect_true(all(pred2_y > 0))

  # Expect errors since increment cannot be zero or negative.
  dd <- dat
  dat$y1 <- dat$y2
  expect_error(dat |> fit_growth(fo, log_transf = T, positive_rate = FALSE, verbose = F))

  dat <- dd
  dat$y1 <- 1:100
  dat$y2 <- 1:100 + rnorm(100)
  dat$temp <- 1:100
  dat$prec <- 1:100
  expect_error(dat |> fit_growth(fo, log_transf = T, positive_rate = FALSE, verbose = F))


  # Real data for Q. ilex and P. halepensis.
  data(treesIFN)

  species <- c("Quercus ilex", "Pinus halepensis")
  for (i in species) {

    dat <- treesIFN[treesIFN$species == i, ]
    dat$y2 <- dat$dbh3
    dat$y1 <- dat$dbh2
    dat$intercept <- rep(1, nrow(dat))
    dat$tdiff <- rep(10, nrow(dat))
    dat$max_y <- 150

    fo <- ~ intercept + y1 + temp + prec

    if (i == "Pinus halepensis") {
      expect_message(r4 <- dat |> fit_growth(fo, log_transf = FALSE, positive_rate = FALSE, verbose = F))
    } else {
      expect_no_condition(r4 <- dat |> fit_growth(fo, log_transf = FALSE, positive_rate = FALSE, verbose = F))
    }
    if (i == "Pinus halepensis") {
      expect_error(expect_message(r5 <- dat |> fit_growth(fo, log_transf = TRUE, positive_rate = FALSE, verbose = F)))
    } else {
      expect_no_condition(r5 <- dat |> fit_growth(fo, log_transf = TRUE, positive_rate = FALSE, verbose = F))
    }
    expect_no_condition(r6 <- dat |> fit_growth(fo, log_transf = FALSE, positive_rate = TRUE, verbose = F))
    expect_no_condition(r7 <- dat |> fit_growth(fo, log_transf = TRUE, positive_rate = TRUE, verbose = F))

    if (i != "Pinus halepensis") sd5 <- sd(resid(r5))
    sd7 <- sd(resid(r7))

    if (i != "Pinus halepensis") expect_gt(cor(dat$y2-dat$y1, predict(r4)), .2)
    if (i != "Pinus halepensis") expect_gt(cor(dat$y2-dat$y1, exp(predict(r5) + .5*sd5^2)), .2)
    expect_gt(cor(dat$y2-dat$y1, predict(r6)), .2)
    expect_gt(cor(dat$y2-dat$y1, exp(predict(r7) + .5*sd7^2)), .2)

  }

})
