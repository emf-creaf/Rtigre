#' Time-independent growth curve
#'
#' Calculation of a time-independent growth curve from parameters.
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
#' @return value of time-independent growth curve for each row in 'dat'.
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
#' plot(y1,ti_growth(dat),xlab="Size at t1",ylab="Growth at t2",type="l",ylim=c(0,50))
#' points(y1,ti_growth(dat,"schumacher"),type="l",lty=2)
#' points(y1,ti_growth(dat,"gompertz"),type="l",lty=3)
#' points(y1,ti_growth(dat,"monomolecular"),type="l",lty=4)
#' points(y1,ti_growth(dat,"arctangent"),type="l",lty=5)
#' points(y1,ti_growth(dat,"hyperbolic"),type="l",lty=6)
#' legend("topright",lty=1:6,c("logistic","schumacher","gompertz","monomolecular",
#' "arctangent","hyperbolic"),lwd=2,cex=1.1)
#'
#'
#' @export

ti_growth <- function(dat, type = "logistic", expression = F) {

  if (!is.data.frame(dat)) stop("'dat' must be a data.frame")
  if (!any(type==c("logistic","schumacher","gompertz","monomolecular","arctangent","hyperbolic")))
    stop("Wrong 'type' value")

  type <- tolower(type)
  if (expression) {
    y2 <- switch(type,
                 "logistic"      = with(dat, max_y/(1+exp(-k*tdiff)*(max_y/y1-1))),
                 "schumacher"    = with(dat, max_y*exp(-(1/(1/log(max_y/y1)+k*tdiff)))),
                 "gompertz"      = with(dat, max_y*(y1/max_y)^exp(-k*tdiff)),
                 "monomolecular" = with(dat, max_y-(max_y-y1)*exp(-k*tdiff)),
                 "arctangent"    = with(dat, max_y*(atan(tan((y1/max_y-0.5)*pi)+k*tdiff)/pi+0.5)),
                 "hyperbolic"    = with(dat, max_y*(y1/(y1+(max_y-y1)*exp(-2*k*tdiff))))
    )

  } else {
    y2 <- switch(type,
                 "logistic"      = with(dat, max_y/(1+exp(-k*tdiff)*(max_y/y1-1))),
                 "schumacher"    = with(dat, max_y*exp(-(1/(1/log(max_y/y1)+k*tdiff)))),
                 "gompertz"      = with(dat, max_y*(y1/max_y)^exp(-k*tdiff)),
                 "monomolecular" = with(dat, max_y-(max_y-y1)*exp(-k*tdiff)),
                 "arctangent"    = with(dat, max_y*(atan(tan((y1/max_y-0.5)*pi)+k*tdiff)/pi+0.5)),
                 "hyperbolic"    = with(dat, max_y*(y1/(y1+(max_y-y1)*exp(-2*k*tdiff))))
    )
  }

  return(y2-y1)

}
