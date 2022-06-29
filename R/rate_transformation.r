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
#' Column 'max_y' correspond to maximum size attainable by the individual when time tends to infinite. This must be known a priori
#' and is not calculated by the function.
#' Details about the six growth curves are given in the accompanying vignettes.
#'
#' @return value of rate parameter for each row in 'dat'.
#'
#' @examples
#'
#' # In the examples below we will simulate a time-dependent growth curve and
#' # then we will recover the input k-value.
#'
#' ## Common parameters.
#' tdiff <- 1
#' t <- seq(1,100,by=tdiff)
#' max_y <- 5.7
#' k <- .05
#' y <- max_y/(1+exp(-(k*t-2))) + rnorm(length(t))*.01
#' plot(t,y,xlab="Time",ylab="Size")
#'
#' ## Logistic growth.
#' dat <- data.frame(tdiff=tdiff,max_y=max_y)
#' hist(rate_transformation(cbind(dat,y1=y[-length(t)],y2=y[-1]),
#' curve_type = "logistic"),breaks=20,
#' xlab="k parameter",main="")
#'
#' ## Actual Pinus uncinata data from the Spanish Forest Inventories.
#' data("Punci_IFN")
#'
#' ## Add time difference between second and third Inventory.
#' Punci_IFN$tdiff <- 10
#'
#' ## Show how growth rate decreases as trees get larger.
#' k <- rate_transformation(Punci_IFN)
#' plot(Punci_IFN$y1,k,log="y",xlab="Tree diameter (cm)",ylab="Growth rate")
#'
#' # Other plots.
#' plot(Punci_IFN$prec,k,log="y",xlab="Precipitation (mm)",ylab="Growth rate")
#' plot(Punci_IFN$temp,k,log="y",xlab="Temperature (ºC)",ylab="Growth rate")
#'
#'@references
#' Burkhart, Harold E., and Margarida Tomé. "Growth functions." In Modeling forest trees and stands,
#' pp. 111-130. Springer, Dordrecht, 2012.
#'
#' @export

rate_transformation <- function(dat, curve_type = "logistic") {

  cl <- match.call()
  m <- match(c("dat"),names(cl))
  if (any(is.na(m))) stop("Missing argument")
  if (!is.data.frame(dat)) stop("'dat' must be a data.frame")
  if (any(is.na(match(c("y1","y2","tdiff","max_y"),colnames(dat))))) stop("Wrong column names")

  if (!any(curve_type==c("logistic","schumacher","gompertz","monomolecular","arctangent","hyperbolic")))
    stop("Wrong 'curve_type' value")

  if (tolower(curve_type) == "logistic") {
    k <- with(dat, eval(parse(text = gr_logistic(equation_type = "rate"))))

  } else if (curve_type == "schumacher") {
    k <- with(dat, 1/(tdiff*(1/log(max_y/y2)-1/log(max_y/y1))))

  } else if (curve_type == "gompertz") {
    k <- with(dat, 1/tdiff*log(log(max_y/y1)/log(max_y/y2)))

  } else if (curve_type == "monomolecular") {
    k <- with(dat, 1/tdiff*log((max_y-y1)/(max_y-y2)))

  } else if (curve_type == "arctangent") {
    k <- with(dat, 1/tdiff*(tan((y2/max_y-0.5)*pi)-tan((y1/max_y-0.5)*pi)))

  } else if (curve_type == "hyperbolic") {
    k <- with(dat,1/(2*tdiff)*log((y2/y1)*((max_y-y1)/(max_y-y2))))
  }

  return(k)
}
