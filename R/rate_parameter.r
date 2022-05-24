#' Calculation of rate parameter values for time-independent growth equations.
#'
#' Given two values ***
#'
#' @param dat \code{data.frame} containing at least four columns named 'y1', 'y2', 'tdiff' and 'max_y'. See Details
#' and accompanying Vignettes for a description.
#' @param type string to select which growth function to use. It can be equal to 'logistic',
#' 'schumacher', 'monomolecular', 'gompertz' or 'hypergolic'.
#'
#' @details The growth equations are taken from Table 6.2 in
#' Columns 'y1' and 'y2' in 'dat' correspond to size of the individual at 't1' and 't2', with column 'tdiff'='t2'-'t1'.
#' Column 'max_y' correspond to maximum size attainable by the individual when time tends to infinite.
#'
#' @return value of rate parameter for each row in 'dat'.
#'
#' @examples
#'
#' # In the examples below we will calculate a time-dependent growth curve and will
#' # recover the input k-value.
#'
#' ## Common parameters.
#' tdiff <- 1
#' t <- seq(1,100,by=tdiff)
#' max_y <- 5.5
#' k <- .05
#' dat <- data.frame(tdiff=tdiff,max_y=max_y)
#'
#' ## Logistic growth.
#' y <- max_y/(1+10*exp(-k*t))
#' plot(rate_parameter(cbind(dat,y1=y[-100],y2=y[-1]), type = "logistic"))
#'
#' ## Other growth functions would give different values.
#' plot(rate_parameter(dat, type = "schumacher"))
#' plot(rate_parameter(dat, type = "monomolecular"))
#' plot(rate_parameter(dat, type = "gompertz"))
#'
#' # Notice that the result must be divided by 2. See accompanying vignette.
#' b <- -2
#' y <- max_y*tanh(k*t+b)
#' dat <- data.frame(y1=y[-100],y2=y[-1],tdiff=tdiff,max_y=max_y)
#' plot(rate_parameter(dat, type = "hyperbolic"))
#'
#' @export
#'

rate_parameter <- function(data=dat, type = "logistic", sigmoidal = T) {

  type <- tolower(type)
  if (type == "logistic") {
    k <- with(dat, 1/tdiff*log((max_y-y1)/(max_y-y2)*(y2/y1)))
  } else if (type == "schumacher") {
    k <- with(dat, tdiff/(1/log(max_y/y2)-1/log(max_y/y1)))
  } else if (type == "monomolecular") {
    k <- with(dat, 1/tdiff*log((max_y-y1)/(max_y-y2)))
  } else if (type == "gompertz") {
    k <- with(dat, 1/tdiff*log(log(max_y/y1)/log(max_y/y2)))
  } else if (type == "hyperbolic") {
    k <- with(dat,1/tdiff*log((max_y+y2)/(max_y+y1)*(max_y-y1)/(max_y-y2)))
  } else stop("Wrong 'type' value")

  return(k)
}
