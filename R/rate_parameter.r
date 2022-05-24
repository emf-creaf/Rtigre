rate_parameter <- function(data=dat, type = "logistic") {

  if (type == "logistic") {
    k <- with(dat, 1/tdiff*log((max_y-y1)/(max_y-y2)*(y2/y1)))
  } else if (type == "schumacher") {
    k <- with(dat, tdiff/(1/log(max_y/y2)-1/log(max_y/y1)))
  } else if (type == "monomolecular") {
    k <- with(dat, 1/tdiff*log((max_y-y1)/(max_y-y2)))
  } else if (type == "Richards") {
    k <- with(dat, )
  }
  return(k)
}
