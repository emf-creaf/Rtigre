test_that("multiplication works", {


  # source("./R/fit_growth.R")
  # source("./R/helpers.R")
  # source("./R/fit_rate.R")
  # source("./R/rate_gr.R")
  # source("./R/eval_gr.R")
  # source("./R/string_gr.R")
  # source("./R/gr_logistic.R")


  ## Common parameters. Simple example.
  tdiff <- 5
  t <- seq(1,100,by=tdiff)
  max_y <- 120
  k <- .1

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
  dat <- data.frame(tdiff = t[2] - t[1], max_y = max_y, y1 = y1, y2 = y2, temp = temp, prec = prec)
  fo <- ~ y1 + prec + temp

  # Check that everything is ok.
  expect_s3_class(dat |> fit_growth(fo, log_transf = F, sigmoid_rate = F, verbose = F), "nls")
  expect_s3_class(dat |> fit_growth(fo, log_transf = T, sigmoid_rate = F, verbose = F), "nls")
  expect_s3_class(dat |> fit_growth(fo, log_transf = F, sigmoid_rate = T, verbose = F), "nls")
  expect_s3_class(dat |> fit_growth(fo, log_transf = T, sigmoid_rate = T, verbose = F), "nls")

  # Expect errors.
  dd <- dat
  dat$y1 <- dat$y2
  expect_error(dat |> fit_growth(fo, log_transf = T, sigmoid_rate = T, verbose = F))

  dat <- dd
  dat$y1 <- 1:100
  dat$y2 <- 1:100 + rnorm(100)
  dat$temp <- 1:100
  dat$prec <- 1:100
  expect_warning(expect_error(dat |> fit_growth(fo, log_transf = T, sigmoid_rate = T, verbose = F)))

  ## Same data, but simulating a sigmoid rate.
  # k <- 2/(1+exp(-(intercept+coef_temp*temp+coef_prec*prec)))+rnorm(length(temp))*.001
  # k <- 2/(1+exp(-(intercept+coef_temp*temp+coef_prec*prec)))+rnorm(length(temp))*.001
  # y1 <- max_y/(1+exp(-(k*t[1]-2)))
  # y2 <- max_y/(1+exp(-(k*t[2]-2)))
  # dat <- data.frame(tdiff=t[2]-t[1],max_y=max_y,y1=y1,y2=y2,temp=temp,prec=prec)


  ## Actual Pinus uncinata data from the Spanish Forest Inventories.
  # data("Punci_IFN")

  ## Add time difference between second and third Inventory.
  # Punci_IFN$tdiff <- 10

  # r1 <- fit_growth(Punci_IFN, ~prec+temp, log_transf = T)
  # r2 <- fit_growth(Punci_IFN, ~prec+temp, algorithm = "nls", log_transf = T)
  # r3 <- fit_growth(Punci_IFN, ~prec+temp, algorithm = "nlsr", log_transf = T)
  #
  # summary(r1)



})
