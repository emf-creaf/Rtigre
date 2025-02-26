test_that("Choice of growth curve", {

  for (i in c("td", "ti", "rate")) {
    expect_identical(string_gr("logistic", i), gr_logistic(i))
    expect_identical(string_gr("rational", i), gr_rational(i))
    expect_identical(string_gr("schumacher", i), gr_schumacher(i))
    expect_identical(string_gr("monomolecular", i), gr_monomolecular(i))
    expect_identical(string_gr("monomolecular2", i), gr_monomolecular2(i))
    expect_identical(string_gr("hyperbolic", i), gr_hyperbolic(i))
    expect_identical(string_gr("gompertz", i), gr_gompertz(i))
    expect_identical(string_gr("arctangent", i), gr_arctangent(i))
    expect_identical(string_gr("arctangent_exp", i), gr_arctangent_exp(i))
  }

})
