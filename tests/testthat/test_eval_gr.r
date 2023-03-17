test_that("sdf", {

   dat <- data.frame(y1 = 10, y2 = 43, tdiff = 10, max_y = 80, t = 25, k = .025, offset = -.1)
   eval_gr(dat, "logistic", "td")
   eval_gr(dat, "logistic", "rate")
   eval_gr(dat, "logistic", "ti")

   # Tests results.
   expect_lt(abs(eval_gr(dat, "logistic", "td") - 50.2653),.01)
   expect_lt(abs(eval_gr(dat, "logistic", "rate") - 0.2096192),.01)
   expect_lt(abs(eval_gr(dat, "logistic", "ti") - 12.40001),.01)

   # Test errors.
   expect_error(eval_gr(dat, "what?", "td"))

})
