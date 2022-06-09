#' Rate parameter values in time-independent growth equations.
#'
#' This function calculates the rate parameter value of several time-independent growth equations from two size values
#' obtained at two different times.
#'
#' @param dat \code{data.frame} containing at least four columns named 'y1', 'y2', 'tdiff' and 'max_y'. See Details
#' and accompanying Vignettes for a description.
#' @param type string to select which growth function to use. It can be equal to 'logistic',
#' 'schumacher', 'monomolecular', 'gompertz', 'arctangent' or 'hyperbolic'.
#'
#' @details Some of the growth equations are taken from Table 6.2 in
#' Burkhart and Tomé (2012).
#' Columns 'y1' and 'y2' in 'dat' correspond to the sizes of the individual at 't1' and 't2', with column 'tdiff'='t2'-'t1'.
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
#' max_y <- 5.7
#' k <- .05
#' dat <- data.frame(tdiff=tdiff,max_y=max_y)
#'
#' ## Logistic growth.
#' y <- max_y/(1+10*exp(-k*t))+runif(length(t))*.01
#' hist(rate_parameter(cbind(dat,y1=y[-length(t)],y2=y[-1]), type = "logistic"),breaks=20)
#'
#'@references
#' Burkhart, Harold E., and Margarida Tomé. "Growth functions." In Modeling forest trees and stands,
#' pp. 111-130. Springer, Dordrecht, 2012.
#'
#' @export

rate_parameter <- function(dat, type = "logistic") {

  cl <- match.call()
  m <- match(c("dat"),names(cl))
  if (any(is.na(m))) stop("Missing argument")
  if (!is.data.frame(dat)) stop("'dat' must be a data.frame")
  if (any(is.na(match(c("y1","y2","tdiff","max_y"),colnames(dat))))) stop("Wrong column names")

  if (tolower(type) == "logistic") {
    k <- with(dat, 1/tdiff*log(y2/y1*(max_y-y1)/(max_y-y2)))
  } else if (type == "schumacher") {
    k <- with(dat, 1/(tdiff*(1/log(max_y/y2)-1/log(max_y/y1))))
  } else if (type == "gompertz") {
    k <- with(dat, 1/tdiff*log(log(max_y/y1)/log(max_y/y2)))
  } else if (type == "monomolecular") {
    k <- with(dat, 1/tdiff*log((max_y-y1)/(max_y-y2)))
  } else if (type == "arctangent") {
    k <- with(dat, 1/tdiff*(tan((y2/max_y-0.5)*pi)-tan((y1/max_y-0.5)*pi)))
  } else if (type == "hyperbolic") {
    k <- with(dat,1/(2*tdiff)*log(((max_y+y2)/(max_y+y1))*((max_y-y1)/(max_y-y2))))
  } else stop("Wrong 'type' value")

  return(k)
}
