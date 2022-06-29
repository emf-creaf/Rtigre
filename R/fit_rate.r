#' Regression of rate parameter
#'
#' This function computes a regression of the rate parameters against a set of predictor variables.
#' obtained at two different times.
#'
#' @param dat \code{data.frame} containing at least four columns named 'y1', 'y2', 'tdiff' and 'max_y'. See Details
#' and accompanying Vignettes for a description.
#' @param fo an object of class "formula"
#' @param type string to select which growth function to use. It can be equal to 'logistic',
#' 'schumacher', 'monomolecular', 'gompertz', 'arctangent' or 'hyperbolic'.
#' @param sigmoid logical. If TRUE, the growth rate 'k' is further modelled as a logistic function.
#' @param kmax numeric. If NULL, it will be estimated from the data.
#'
#' @details Some of the growth equations are taken from Table 6.2 in #' Burkhart and Tomé (2012).
#' Columns 'y1' and 'y2' in 'dat' correspond to the sizes of the individual at 't1' and 't2', with column 'tdiff'='t2'-'t1'.
#' Column 'max_y' correspond to maximum size attainable by the individual when time tends to infinite.
#'
#' The 'sigmoid' option allows us to guarantee that \code{k} is never negative and it has an upper limit,
#' some that is ecologically sound in most cases. Negative \code{k} values may happen when growth curves
#' are used to calculate growth under conditions much different from the initial ones.
#'
#' @return value of rate parameter for each row in 'dat'.
#'
#' @examples
#'
#' # In the examples below we will calculate a time-dependent growth curve and will
#' # recover the input k-value.
#'
#' ## Common parameters. Simple example.
#' tdiff <- 5
#' t <- seq(1,100,by=tdiff)
#' max_y <- 120
#' k <- .1
#'
#' y <- max_y/(1+exp(-(k*t-2))) + rnorm(length(t))*.01
#' plot(t,y,xlab="Time",ylab="Size")
#'
#' dat <- data.frame(tdiff=tdiff,max_y=max_y,y1=y[-length(t)],y2=y[-1])
#' r1 <- fit_rate(dat,~1)
#'
#' ## Fake climatic data.
#' temp <- runif(100,18.6,21.3)
#' prec <- runif(100,359,514)
#' t <- c(10,20)
#' intercept <- .02
#' coef_temp <- .00061
#' coef_prec <- .000052
#' k <- intercept+coef_temp*temp+coef_prec*prec+rnorm(length(temp))*.001
#' y1 <- max_y/(1+exp(-(k*t[1]-2)))
#' y2 <- max_y/(1+exp(-(k*t[2]-2)))
#' dat <- data.frame(tdiff=t[2]-t[1],max_y=max_y,y1=y1,y2=y2,temp=temp,prec=prec)
#'
#' ## Logistic growth.
#' r2 <- fit_rate(dat,~1)
#' summary(r2)
#'
#' ## A better model.
#' r3 <- fit_rate(dat,~temp+prec)
#' summary(r3)
#'
#' ## Assuming a sigmoid expression for k.
#' r4 <- fit_rate(dat,~temp+prec, sigmoid=T)
#' summary(r4)
#'
#' ## Actual Pinus uncinata data from the Spanish Forest Inventories.
#' data("Punci_IFN")
#'
#' ## Add time difference between second and third Inventory.
#' Punci_IFN$tdiff <- 10
#'
#' k <- fit_rate(Punci_IFN, ~prec+temp, sigmoid = T)
#' summary(k)
#'
#' @references
#' Burkhart, Harold E., and Margarida Tomé. "Growth functions." In Modeling forest trees and stands,
#' pp. 111-130. Springer, Dordrecht, 2012.
#'
#' @export

fit_rate <- function(dat, fo, curve_type = "logistic", sigmoid = F, kmax = NULL) {

  cl <- match.call()
  m <- match(c("dat","fo"),names(cl))
  if (any(is.na(m))) stop("Missing argument")

  tf <- terms.formula(fo)
  is.intercept <- attr(tf,"intercept")
  if (length(is.intercept)==0) stop("Expression in formula 'fo' must have an intercept term")

  if (!any(curve_type==c("logistic","schumacher","gompertz","monomolecular","arctangent","hyperbolic")))
    stop("Wrong 'curve_type' value")

  # Transformation to build a linear expression for k.
  # dat <- cbind(dat,k=rate_parameter(dat, type = type))
  dat$k <- rate_transformation(dat, curve_type = curve_type)

  # sigmoid = TRUE only makes sense if there are predictor variables in the formula.
  if (sigmoid) {
    if (is.null(kmax)) kmax <- max(dat$k)*1.05 # A bit larger.
    dat$k <- log(dat$k/(kmax-dat$k))    # Further logit transformation.
  }

  r <- lm(update(fo,k~.),data=dat)
  if (sigmoid) r$kmax <- kmax

  return(r)
}

