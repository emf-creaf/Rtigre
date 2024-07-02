#' Non-linear fit to a time-independent growth curve.
#'
#' @description
#' Non-linear least-squares fit of growth data to a time-independent growth curve.
#'
#' @param dat \code{data.frame} containing all data to be used in the fit.
#' @param fo \code{formula} describing the right-hand-side of the dependence of the
#' growth rate on the predictors.
#' @param sigmoid_rate logical. If TRUE, growth rate will also be represented by a logistic.
#' @param curve_type character. It indicates the type of growth curve to be used.
#' @param kmax NULL or numeric. If NULL, its value will be calculated from the
#' data.
#' @param log_transf logical. If TRUE, a log-transformation will be applied to
#' the dependent variable.
#' @param verbose logical. If TRUE, information on the progress of the regression is produced.
#' @param algorithm character, the algorithm to be used. See 'Details'.
#'
#' @return a \code{nls} object. See \link[stats]{nls} for details.
#'
#' @details
#' It is challenging to figure good starting values for a non-linear fit. When the algorithm implemented
#' in e.g. \link[stats]{nls} does not work (which is often the case), one can then turn to using a
#' slower but safer method like e.g. "Nelder-Mead", or  of the \link[stats]{nls}
#'
#' The algorithm first fits an explicit expression of the growth rate as a function of the
#' explanatory variables. The resulting coefficients will be the starting values for the
#' final non-linear regression.
#' Be aware that, in some cases, \code{sigmoid = T} may give rise to "singular gradient" error messages in
#' \link[stats]{nls} or some other error and/or warning messages for \link[minpack.lm]{nlsLM}
#' or \link[nlsr]{nlsr}.
#' The default algorithm is "nlsLM", which implements the "nlsLM" function in package
#' \code{minpack}. Other allowed values are "nls", which used the built-in \code{nls}
#' function and "nlsr" from package \code{nlsr}. The Levenberg-Marquardt in \link[minpack.lm]{nlsLM}
#' is very fast and stable and should work fine most of the time.
#'
#' @export
#'
#' @examples
#'
#' ## Common parameters. Simple example.
#' tdiff <- 5
#' t <- seq(1,100,by=tdiff)
#' max_y <- 120
#' k <- .1
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
#'
#' ## Same data, but simulating a sigmoid rate.
#' k <- 2/(1+exp(-(intercept+coef_temp*temp+coef_prec*prec)))+rnorm(length(temp))*.001
#' y1 <- max_y/(1+exp(-(k*t[1]-2)))
#' y2 <- max_y/(1+exp(-(k*t[2]-2)))
#' dat <- data.frame(tdiff=t[2]-t[1],max_y=max_y,y1=y1,y2=y2,temp=temp,prec=prec)
#'
#'
#' ## Actual Pinus uncinata data from the Spanish Forest Inventories.
#' data("Punci_IFN")
#'
#' ## Add time difference between second and third Inventory.
#' Punci_IFN$tdiff <- 10
#'
#' r <- fit_growth(Punci_IFN, ~prec+temp)
#' summary(r)
#'
#'
fit_growth <- function(dat, fo, curve_type = "logistic", sigmoid_rate = F, kmax = NULL, algorithm = "nlsLM", log_transf = T, verbose = T) {


  # Checks.
  stopifnot("Input 'dat' must be a 'data.frame'" = is.data.frame(dat))
  stopifnot("Input 'fo' must be a 'formula'" = inherits(fo, "formula"))
  curve_type = match.arg(curve_type, c("logistic","schumacher","gompertz","monomolecular","arctangent","hyperbolic", "user"))
  algorithm <- match.arg(algorithm, c("nlsLM", "nls", "nlsr"))
  if (!is.logical(verbose)) stop("Input 'verbose' must be logical")


  # Check components in formula.
  cl <- match.call()
  m <- match(c("dat","fo"),names(cl))
  if (any(is.na(m))) stop("Missing argument")
  tf <- terms.formula(fo)
  is.intercept <- attr(tf,"intercept")
  if (length(is.intercept)==0) stop("Expression in formula 'fo' must have an intercept term")


  # Need info on the screen?
  if (verbose) {
    out <- paste0("Growth regression - ", curve_type, " curve")
    if (sigmoid_rate) out <- paste0(out," - sigmoid rate")
    if (log_transf) out <- paste0(out, " - log-transformation")
    cat(paste0(out,"\n"))
  }

  # Get regression parameters.
  if (verbose) cat("   Linear regression of growth rate against predictors\n")
  r <- fit_rate(dat = dat, fo = fo, curve_type = curve_type, sigmoid_rate = sigmoid_rate, kmax = kmax)
  coef_start <- coef(r)


  # Intercept term.
  x <- names(coef_start)[1]
  names_start <- "Intercept"


  # If fo contains more predictors, add them to the formula string.
  if (length(coef_start) > 1) {
    for (i in 2:length(coef_start)) {
      x <- paste0(x,"+coef_",names(coef_start)[i],"*",names(coef_start)[i])
      names_start <- c(names_start,paste0("coef_",names(coef_start)[i]))
    }
  }
  names(coef_start) <- names_start


  # If we opted for a sigmoid formula...
  if (sigmoid_rate) {
    x <- paste0("kmax/(1+exp(-(",x,")))")
    coef_start <- c(r$kmax, coef_start)
    names(coef_start)[1] <- "kmax"
  }


  # Next, we build the formula. We leave it as a string in case a log-transformation
  # is required below. It's easy to work with strings in that case.
  x <- paste0("(",x,")")
  y <- string_gr(curve_type, "ti")
  z <- gsub("k",x,y)
  fofo <- paste0("y2-y1~",z,"-y1")


  # The non-linear fit.
  if (verbose) cat("   Non-linear fit\n")
  r <- switch(algorithm,
              nlsLM = minpack.lm::nlsLM(formula(fofo), data = dat, start = coef_start, control = list(maxiter = 1000)),
              nls = nls(formula(fofo), data = dat, start = coef_start, control = list(maxiter = 1000)),
              nlsr = nlsr::nlsr(formula(fofo), data = dat, start = coef_start)
  )


  # If a log-transformed regression is sought.
  if (log_transf) {
    if (verbose) cat("   Non-linear fit of log-transformed data\n")
    fofo <- paste0("log(y2-y1)~log(", z, ")")
    r <- switch(algorithm,
                nlsLM = minpack.lm::nlsLM(formula(fofo), data = dat, start = coef_start, control = list(maxiter = 10000)),
                nls = nls(formula(fofo), data = dat, start = coef_start, control = list(maxiter = 10000)),
                nlsr = nlsr::nlsr(formula(fofo), data = dat, start = coef_start)
    )
  }

  return(r)
}
