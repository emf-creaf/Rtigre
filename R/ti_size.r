#' Time-independent size calculation
#'
#' @description
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
#' dat <- data.frame(tdiff=tdiff,max_y=max_y,k=k,y1=y1)
#' plot(y1,Rtigre::ti_size(dat),xlab="Size at t1",ylab="Size at t2",type="l",lwd=2,col=1,
#'   ylim=c(0,max_y))
#' points(y1,Rtigre::ti_size(dat,"schumacher"),type="l",lwd=2,col=2)
#' points(y1,Rtigre::ti_size(dat,"gompertz"),type="l",lwd=2,col=3)
#' points(y1,Rtigre::ti_size(dat,"monomolecular"),type="l",lwd=2,col=4)
#' points(y1,Rtigre::ti_size(dat,"arctangent"),type="l",lwd=2,col=5)
#' points(y1,Rtigre::ti_size(dat,"hyperbolic"),type="l",lwd=2,col=6)
#' legend("topright",lty=1,c("logistic","schumacher","gompertz","monomolecular",
#' "arctangent","hyperbolic"),lwd=2,cex=1.1,col=1:6)
#'
#' # To plot growth as y2-y1.
#' plot(y1,Rtigre::ti_size(dat)-dat$y1,xlab="Size at t1",ylab="Growth at t2",type="l",lwd=2,col=1,
#'   ylim=c(0,max_y))
#' points(y1,Rtigre::ti_size(dat,"schumacher")-dat$y1,type="l",lwd=2,col=2)
#' points(y1,Rtigre::ti_size(dat,"gompertz")-dat$y1,type="l",lwd=2,col=3)
#' points(y1,Rtigre::ti_size(dat,"monomolecular")-dat$y1,type="l",lwd=2,col=4)
#' points(y1,Rtigre::ti_size(dat,"arctangent")-dat$y1,type="l",lwd=2,col=5)
#' points(y1,Rtigre::ti_size(dat,"hyperbolic")-dat$y1,type="l",lwd=2,col=6)
#' legend("topright",lty=1,c("logistic","schumacher","gompertz","monomolecular",
#' "arctangent","hyperbolic"),lwd=2,cex=1.1,col=1:6)
#'
#' @export

ti_size <- function(dat, curve_type = "logistic") {


  # Checks.
  stopifnot("Input 'dat' must be a 'data.frame'" = is.data.frame(dat))
  stopifnot("Wrong 'curve_type'" = any(curve_type %in% c("logistic", "schumacher", "gompertz",
                                                         "monomolecular", "arctangent",
                                                         "hyperbolic", "arctangent_exp",
                                                         "rational", "user")))


  y <- eval_gr(dat = dat, curve_type = curve_type, equation_type = "ti")


  return(y)

}
