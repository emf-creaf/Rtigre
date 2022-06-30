#' Time-dependent size calculation
#'
#' This function calculates size as a function of time for seven sigmoid functions.
#'
#' @param dat \code{data.frame} containing at least five columns named 't', 'k', 'max_y' and 'offset'. See Details
#' and accompanying Vignettes for a description.
#' @param type string to select which growth function to use. It can be equal to 'logistic',
#' 'schumacher', 'monomolecular', 'gompertz', 'arctangent' or 'hyperbolic'.
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
#' plot(t,growth_curve(dat,type="logistic"),type="l",lty=1,lwd=2,ylim=c(0,max_y))
#' points(t,growth_curve(dat,"schumacher"),type="l",lty=2,lwd=2)
#' points(t,growth_curve(dat,type="gompertz"),type="l",lty=3,lwd=2)
#' points(t,growth_curve(dat,type="monomolecular"),type="l",lty=4,lwd=2)
#' points(t,growth_curve(dat,type="arctangent"),type="l",lty=5,lwd=2)
#' points(t,growth_curve(dat,type="hyperbolic"),type="l",lty=6,lwd=2)
#' legend("bottomright",lty=1:6,c("logistic","schumacher","gompertz","monomolecular",
#' "arctangent","hyperbolic"),lwd=2,cex=1.1)
#'
#'@references
#' Burkhart, Harold E., and Margarida Tomé. "Growth functions." In Modeling forest trees and stands,
#' pp. 111-130. Springer, Dordrecht, 2012.
#'
#' @export

td_size <- function(dat, curve_type = "logistic") {

  cl <- match.call()
  m <- match(c("dat"),names(cl))
  if (any(is.na(m))) stop("Missing argument")
  if (!is.data.frame(dat)) stop("'dat' must be a data.frame")
  if (nrow(dat) == 0) stop("'dat' must have at least one row")
  if (any(is.na(match(c("t","max_y","k","offset"),colnames(dat))))) stop("Wrong column names")

  if (!any(curve_type==c("logistic","schumacher","gompertz","monomolecular","arctangent","hyperbolic", "user")))
    stop("Wrong 'curve_type' value")

  y <- eval_growth(dat = dat, curve_type = curve_type, equation_type = "td")

  return(y)

}
