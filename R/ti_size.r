#' Time-independent size calculation
#'
#' Calculation of a time-independent size curve from parameters.
#'
#' @param dat \code{data.frame} containing at least four columns named 'y1', 'tdiff', 'max_y' and 'k'. See Details
#' and accompanying Vignettes for a description.
#' @param type string to select which growth function to use. It can be equal to 'logistic',
#' 'schumacher', 'monomolecular', 'gompertz' or 'hyperbolic'.
#'
#' @details Growth is defined as the increment in dimension between t1 and t2, with t1<t2.
#' The growth equations are taken from Table 6.2 in
#' Column 'y1' 'dat' corresponds to size of the individual at 't1', with column 'tdiff'='t2'-'t1'.
#' Column 'max_y' correspond to maximum size attainable by the individual when time tends to infinite.
#' Column 'k' is the rate parameter of the growth curve.
#'
#' @return new size after a time interval 'tdiff'.
#'
#' @examples
#'
#' ## Common parameters.
#' tdiff <- 5
#' max_y <- 120
#' y1 <- seq(1,110)
#' k <- .1
#'
#' ## Logistic growth.
#' dat <- data.frame(tdiff=tdiff,max_y=max_y,k=k,y1=y1)
#' plot(y1,ti_size(dat),xlab="Size at t1",ylab="Growth at t2",type="l",ylim=c(0,50))
#' points(y1,ti_size(dat,"schumacher"),type="l",lty=2)
#' points(y1,ti_size(dat,"gompertz"),type="l",lty=3)
#' points(y1,ti_size(dat,"monomolecular"),type="l",lty=4)
#' points(y1,ti_size(dat,"arctangent"),type="l",lty=5)
#' points(y1,ti_size(dat,"hyperbolic"),type="l",lty=6)
#' legend("topright",lty=1:6,c("logistic","schumacher","gompertz","monomolecular",
#' "arctangent","hyperbolic"),lwd=2,cex=1.1)
#'
#'
#' @export

ti_size <- function(dat, curve_type = "logistic") {

  y <- eval_gr(dat = dat, curve_type = curve_type, equation_type = "ti")

  return(y)

}
