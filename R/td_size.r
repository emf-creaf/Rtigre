#' Time-dependent size calculation
#'
#' This function calculates size as a function of time for seven sigmoid functions.
#'
#' @param dat \code{data.frame} containing at least five columns named 't', 'k', 'max_y' and 'offset'. See Details
#' and accompanying Vignettes for a description.
#' @param type string to select which growth function to use. It can be equal to 'logistic',
#' 'schumacher', 'monomolecular', 'gompertz', 'arctangent', 'hyperbolic', 'arctangent_exp'
#' 'rational' or 'user'.
#'
#' @details Some of the growth equations are taken from Table 6.2 in
#' Burkhart and Tomé (2012).
#' Column 'max_y' correspond to maximum size attainable by the individual when time tends to infinite.
#' This must be known a priori and is not calculated by the function.
#' Details about the six growth curves are given in the accompanying vignettes.
#'
#' @return growth value for each 't' value.
#'
#' @examples
#'
#' ## Common parameters.
#' t <- seq(1,100,by=.1)
#' max_y <- 120
#' k <- .1
#' offset <- -2
#' dat <- data.frame(t=t,k=k,offset=offset,max_y=max_y)
#' plot(t,Rtigre::td_size(dat,curve_type="logistic"),type="l",lwd=2,ylim=c(0,max_y),col=1,
#'      xlab="Time",ylab="Size",main="Age/time-independent size")
#' points(t,Rtigre::td_size(dat,curve_type="schumacher"),type="l",lwd=2,col=2)
#' points(t,Rtigre::td_size(dat,curve_type="gompertz"),type="l",lwd=2,col=3)
#' points(t,Rtigre::td_size(dat,curve_type="monomolecular"),type="l",lwd=2,col=4,)
#' points(t,Rtigre::td_size(dat,curve_type="arctangent"),type="l",lwd=2,col=5)
#' points(t,Rtigre::td_size(dat,curve_type="hyperbolic"),type="l",lwd=2,col=6)
#' legend("bottomright",lty=1,c("logistic","schumacher","gompertz","monomolecular",
#'                              "arctangent","hyperbolic"),lwd=2,cex=1.1,col=1:6)
#'
#'@references
#' Burkhart, Harold E., and Margarida Tomé. "Growth functions." In Modeling forest trees and stands,
#' pp. 111-130. Springer, Dordrecht, 2012.
#'
#' @export

td_size <- function(dat, curve_type = "logistic") {


  # Checks.
  stopifnot("Input 'dat' must be a 'data.frame'" = is.data.frame(dat))
  curve_type = match.arg(curve_type, all_curve_types())


  y <- eval_gr(dat = dat, curve_type = curve_type, equation_type = "td")


  return(y)

}
