growth_curve <- function(dat, type = "logistic") {

  if (tolower(type) == "logistic") {
    k <- with(dat, max_y/(1+exp(-(k*t+offset))))
  } else if (type == "schumacher") {
    k <- with(dat, max_y*exp(-1/(k*t+offset)))
  } else if (type == "gompertz") {
    k <- with(dat, max_y*exp(-exp(-(k*t+offset))))
  } else if (type == "monomolecular") {
    k <- with(dat, max_y*(1-exp(-(k*t+offset))))
  } else if (type == "arctangent") {
    k <- with(dat, max_y*(atan(k*t+offset)/pi+0.5))
  } else if (type == "hyperbolic") {
    k <- with(dat, max_y*((exp(2*(k*t+offset))-1)/(exp(2*(k*t+offset))+1)))
  } else stop("Wrong 'type' value")

}
