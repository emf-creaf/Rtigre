#' Regression on rate parameter
#'
#' @description
#' \code{fit_rate} computes a regression of the rate parameters against a set of predictor variables.
#' obtained at two different times.
#'
#' @param dat \code{data.frame} containing at least four columns named 'y1', 'y2', 'tdiff' and 'max_y'. See Details
#' and accompanying Vignettes for a description.
#' @param fo an object of class "formula"
#' @param kmax numeric. If NULL, \code{kmax} will be estimated from the data.
#' @param curve_type
#' @param positive_rate see \code{\link{fit_growth}}.
#'
#' @details Some of the growth equations are taken from Table 6.2 in #' Burkhart and Tomé (2012).
#' Columns  'y1' and 'y2' in 'dat' correspond to the sizes of the individual at 't1' and 't2', with column 'tdiff'='t2'-'t1'.
#' Column 'max_y' correspond to maximum size attainable by the individual when time tends to infinite.
#'
#' The 'positive_rate = TRUE' option allows us to guarantee that \code{k} always remains positive.
#' Negative \code{k} values may happen when growth curves
#' are used to calculate growth under conditions much different from the initial ones.
#'
#' An intercept is not assumed in the regression model. If we seek to include one, add a constant column
#' to the input data.frame 'dat' with 1's and specify this column in the model 'fo' (see example below).
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
#' t <- seq(1, 100, by = tdiff)
#' max_y <- 120
#' k <- .1
#'
#' dat <- data.frame(t = t, max_y = max_y, offset = 2, k = .05, Intercept = 1)
#' y <- td_size(dat, "logistic") + rnorm(length(t))*.01
#' plot(t, y, xlab = "Time", ylab = "Size")
#'
#' dat <- data.frame(tdiff = tdiff, max_y = max_y, y1 = y[-length(t)], y2 = y[-1])
#'
#' # We add an intercept term.
#' dat$Intercept <- 1
#'
#' r1 <- fit_rate(dat, ~ Intercept)
#'
#' ## Fake climatic data.
#' temp <- runif(100, 18.6, 21.3)
#' prec <- runif(100, 359, 514)
#' t <- c(10, 20)
#' Intercept <- .02
#' coef_temp <- .00061
#' coef_prec <- .000052
#' k <- Intercept + coef_temp*temp + coef_prec*prec + rnorm(length(temp))*.001
#' y1 <- max_y/(1+exp(-(k*t[1]-2)))
#' y2 <- max_y/(1+exp(-(k*t[2]-2)))
#' dat <- data.frame(tdiff = t[2]-t[1], max_y = max_y, y1 = y1, y2 = y2, temp = temp, prec = prec, Intercept = 1)
#'
#' ## Logistic growth.
#' r2 <- fit_rate(dat, ~ Intercept)
#' summary(r2)
#'
#' ## A better model.
#' r3 <- fit_rate(dat, ~ Intercept + temp + prec)
#' summary(r3)
#'
#' ## Assuming a sigmoid expression for k.
#' r4 <- fit_rate(dat, ~ Intercept + temp + prec, positive_rate = TRUE)
#' summary(r4)
#'
#' ## Actual Pinus uncinata data from the Spanish Forest Inventories.
#' data("Punci_IFN")
#'
#' ## Add time difference between second and third Inventory.
#' Punci_IFN$tdiff <- 10
#' Punci_IFN$Intercept <- 1
#'
#' k1 <- fit_rate(Punci_IFN, ~ Intercept + prec + temp)
#' k2 <- fit_rate(Punci_IFN, ~ Intercept + prec + temp, positive_rate = TRUE)
#' summary(k1)
#' summary(k2)
#'
#' @references
#' Burkhart, Harold E., and Margarida Tomé. "Growth functions." In Modeling forest trees and stands,
#' pp. 111-130. Springer, Dordrecht, 2012.
#'
#' @export

fit_rate <- function(dat, fo, curve_type = "logistic", positive_rate = FALSE, k_param = NULL) {


  # Checks.
  stopifnot("Input 'dat' must be a 'data.frame'" = is.data.frame(dat))
  stopifnot("Input 'fo' must be a 'formula'" = inherits(fo, "formula"))
  curve_type = match.arg(curve_type, all_curve_types())
  stopifnot("Input 'k_param' must be positive" = k_param > 0)


  # Check formula.
  cl <- match.call()
  m <- match(c("dat", "fo"), names(cl))
  if (any(is.na(m))) stop("Missing argument")


  # Transformation to build a linear expression for k.
  dat$k <- rate_gr(dat, curve_type = curve_type)


  # If 'positive_rate = TRUE' is not NULL, k_param must be specified either on input.
  # If not, a value of 1 is assumed.
  if (positive_rate) {
    if (is.null(k_param)) k_param <- 1
    dat$k <- log(exp(k_param*dat$k)-1)/k_param
  }


  # Linear regression. Intercept-by-default is removed.
  r <- lm(update(fo, k ~ -1 + .), data = dat)


  return(r)
}

