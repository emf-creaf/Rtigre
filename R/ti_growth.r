#' Time-independent growth curve
#'
#' Calculation of a time-independent growth curve from parameters.
#'
#' @param dat \code{data.frame} containing at least four columns named 'y1', 'tdiff', 'max_y' and 'k'. See Details
#' and accompanying Vignettes for a description.
#' @param type string to select which growth function to use. It can be equal to 'logistic',
#' 'schumacher', 'monomolecular', 'gompertz' or 'hyperbolic'.
#'
#' @details The growth equations are taken from Table 6.2 in
#' Column 'y1' 'dat' corresponds to size of the individual at 't1', with column 'tdiff'='t2'-'t1'.
#' Column 'max_y' correspond to maximum size attainable by the individual when time tends to infinite.
#' Column 'k' is the rate parameter of the growth curve.
#'
#' @return value of time-independent growth curve for each row in 'dat'.
#'
#' @examples
#'
#' ## Common parameters.
#' tdiff <- 1
#' t <- seq(1,100,by=tdiff)
#' max_y <- 5.7
#' k <- .05
#' dat <- data.frame(tdiff=tdiff,max_y=max_y)
#'
#' ## Logistic growth.
#' y <- max_y/(1+10*exp(-k*t))+runif(length(t))*.01
#' k <- rate_parameter(cbind(dat,y1=y[-100],y2=y[-1]), type = "logistic")
#' y2 <- ti_growth(cbind(dat,y1=y[-100],k=k))
#'
#' @export

ti_growth <- function(dat, type = "logistic") {

  if (!is.data.frame(dat)) stop("'dat' must be a data.frame")

  type <- tolower(type)
  if (type == "logistic") {
    y2 <- with(dat, max_y/(1+exp(-k*tdiff)*(max_y/y1-1)))
  } else if (type == "schumacher") {

  } else if (type == "monomolecular") {

  } else if (type == "gompertz") {

  } else if (type == "hyperbolic") {

  } else stop("Wrong 'type' value")

  return(y2)

}
