#' Non-linear fit to a time-independent growth curve.
#'
#' @description
#' Non-linear least-squares fit of growth data to a time-independent growth curve.
#'
#' @param dat \code{data.frame} with columns containing the dependent variable and the set
#' of predictors to be used in the fit.
#' @param fo \code{formula} describing the right-hand-side of the dependence of the
#' growth rate on the predictors.
#' @param curve_type character indicating the type of growth curve to be used.
#' @param positive_rate logical parameter to ensure that growth rate 'k' is always positive.
#' If TRUE, growth rate 'k' is approximated by a 'softplus' function. In that case, 'k_param'
#' corresponds to a 'softplus' parameter such that larger 'k_param' values make the elbow
#' of the "softplus" curve more pronounced.
#'
#' @param k_param numeric indicating the value of the softplus transformation parameter.
#' If \code{positive_rate = FALSE} or not set, \code{k_param} is not evaluated.
#' If \code{positive_rate = TRUE} and not set, it will be assumed  \code{k_param=1}.
#'
#' @param log_transf logical. If TRUE, a log-transformation will be applied to
#' the dependent variable.
#' @param verbose logical. If TRUE, information on the progress of the regression is produced.
#' @param algorithm character, the algorithm to be used. See 'Details'.
#'
#' @return a \code{nls} object. See \link[stats]{nls} for details. A 'log_transf' logical attribute is
#' added to the \code{nls} object and is set to the value of the \code{log_transf} parameter.
#'
#' @details
#' The algorithm first fits an explicit expression of the growth rate as a function of the
#' explanatory variables. The resulting coefficients will be the starting values for the
#' final non-linear regression.
#' If \code{positive_rate = FALSE} a softplus transformation is performed to ensure that
#' the growth rate 'k' is always positive.
#' The default non-linear least-squares algorithm is "nlsLM", which is implemented in package
#' \code{minpack}. Other allowed values are "nls", which uses the built-in \code{nls}
#' function and "nlsr" from package \code{nlsr}. The Levenberg-Marquardt in \link[minpack.lm]{nlsLM}
#' is very fast and stable and should work fine most of the time.
#'
#' @export
#'
#' @examples
#'
#' ## Common parameters. Simple example.
#' npoints <- 100
#' tdiff <- 5
#' t <- sample(10:150, npoints, replace = T)
#' max_y <- 120
#'
#' ## Fake climatic data.
#' temp <- rnorm(npoints, mean = 15.4, sd = 1)
#' prec <- rnorm(npoints, mean = 560, sd = 50)
#' Intercept <- .1
#' coef_temp <- -.00061
#' coef_prec <- -.000052
#' k <- Intercept + coef_temp*temp + coef_prec*prec + rnorm(npoints)*.01
#' y1 <- max_y/(1+exp(-(k*t-5)))
#' y2 <- max_y/(1+exp(-(k*(t+tdiff)-5))) + rnorm(npoints)*.01
#' dat <- data.frame(tdiff = tdiff, max_y = max_y, y1 = y1, y2 = y2, temp = temp, prec = prec, Intercept = 1)
#' r <- fit_growth(dat, ~ Intercept + temp + prec, curve_type = "logistic", log_transf = F, positive_rate = F)
#' print(summary(r))
#' plot(with(dat, y2-y1), predict(r), pch = 16, cex = .1, log = "xy")
#' points(c(0.01, 50), c(0.01, 50), type = "l", lwd = 2, col = "red")
#'
#' ## Same data, but forcing growth rate to be strictly positive.
#' r <- fit_growth(dat, ~ Intercept + temp + prec, curve_type = "logistic", log_transf = F, positive_rate = T)
#' print(summary(r))
#' plot(with(dat, y2-y1), predict(r), pch = 16, cex = .1, log = "xy")
#' points(c(0.01, 50), c(0.01, 50), type = "l", lwd = 2, col = "red")
#'
#' ## Actual Pinus uncinata data from the Spanish Forest Inventories.
#' data("Punci_IFN")
#'
#' ## Add time difference between second and third Inventory.
#' Punci_IFN$tdiff <- 10
#'
#' r <- fit_growth(Punci_IFN, ~ prec + temp, log_transf = F, positive_rate = F)
#' print(summary(r))
#' plot(with(Punci_IFN, y2-y1), predict(r), pch = 16, cex = .1, log = "xy")
#' points(c(0.01, 50), c(0.01, 50), type = "l", lwd = 2, col = "red")
#'
#' r <- fit_growth(Punci_IFN, ~ prec + temp, log_transf = F, positive_rate = T)
#' print(summary(r))
#' plot(with(Punci_IFN, y2-y1), predict(r), pch = 16, cex = .1, log = "xy")
#' points(c(0.01, 50), c(0.01, 50), type = "l", lwd = 2, col = "red")
#'
#' r <- fit_growth(Punci_IFN, ~ prec + temp, log_transf = T, positive_rate = T)
#' print(summary(r))
#' plot(with(Punci_IFN, log(y2-y1)), predict(r), pch = 16, cex = .1)
#' points(c(-50, 50), c(-50, 50), type = "l", lwd = 2, col = "red")
#'
fit_growth <- function(dat, fo, curve_type = "logistic", log_transf = FALSE, positive_rate = FALSE, k_param = NULL, algorithm = "nlsLM", verbose = T) {


  # Checks.
  stopifnot("Input 'dat' must be a 'data.frame'" = is.data.frame(dat))
  stopifnot("Input 'fo' must be a 'formula'" = inherits(fo, "formula"))
  stopifnot("Input 'verbose' must be logical" = is.logical(verbose))
  curve_type <- match.arg(curve_type, all_curve_types())
  algorithm <- match.arg(algorithm, c("nlsLM", "nls", "nlsr"))


  # Check that tdiff and observed growth are always positive.
  stopifnot("Values in 'tdiff' column must be all strictly positive" = all(dat$tdiff > 0))
  stopifnot("Difference 'y2-y1' must be always positive" = all((dat$y2-dat$y1) > 0))


  # Check components in formula.
  cl <- match.call()
  m <- match(c("dat","fo"),names(cl))
  if (any(is.na(m))) stop("Missing argument")


  # Need info on the screen?
  if (verbose) {
    out <- paste0("fit_growth: ", curve_type, " curve")
    if (positive_rate) out <- paste0(out,", softplus transformation")
    if (log_transf) out <- paste0(out, ", log-transformation")
    cli::cli_text(out)
  }


  # Get first guess for regression parameters.
  if (verbose) cli::cli_text("fit_growth: linear regression of growth rate against predictors")
  r <- fit_rate(dat = dat, fo = fo, curve_type = curve_type, positive_rate = positive_rate, k_param = k_param)
  coef_start <- coef(r)


  # # If 'fo' contains more predictors, add them to the formula string.
  # Parentheses ")" or "(", and power sign "^", are swapped for an underscore "_" in names.
  # This way R will not stop the execution by complaining about unacceptable parameter names.
  x <- names_start <- NULL
  for (i in 1:length(coef_start)) {
    namcof <- gsub("\\(|\\)", "_", names(coef_start)[i])
    namcof <- gsub("\\^", "_", namcof)
    xx <- ifelse(i == 1, "coef_", " + coef_")
    x <- paste0(x, xx, namcof, "*", names(coef_start)[i])
    names_start <- c(names_start, paste0("coef_", namcof))
  }
  names(coef_start) <- names_start


  # Softplus transformation to ensure k>=0.
  if (positive_rate) {
    if (is.null(k_param)) k_param <- 1
    x <- paste0("log(1+exp(", k_param, " * (", x, ")))/", k_param)
  }


  # Next, we build the formula.
  x <- paste0("(", x, ")")
  y <- string_gr(curve_type, "ti")
  z <- gsub("k", x, y)
  fofo <- paste0("y2-y1 ~ ", z," - y1")


  # The non-linear fit.
  if (verbose) cli::cli_text("fit_growth: non-linear fit")
  r <- switch(algorithm,
              nlsLM = minpack.lm::nlsLM(formula(fofo), data = dat, start = coef_start, control = list(maxiter = 1024)),
              nls = nls(formula(fofo), data = dat, start = coef_start, control = list(maxiter = 1000)),
              nlsr = nlsr::nlsr(formula(fofo), data = dat, start = coef_start)
  )


  # If a log-transformed regression is sought.
  if (log_transf) {
    if (verbose) cli::cli_text("fit_growth: non-linear fit of log-transformed data")
    fofo <- as.formula(paste0("log(y2-y1)~log(", z, " - y1)"))
    coef_start <- coef(r)

    # The non-linear least-squares again.
    r <- tryCatch(switch(algorithm,
                nlsLM = minpack.lm::nlsLM(fofo, data = dat, start = coef_start, control = list(maxiter = 1024)),
                nls = nls(fofo, data = dat, start = coef_start, control = list(maxiter = 1000)),
                nlsr = nlsr::nlsr(fofo, data = dat, start = coef_start)),
                error = function(e) return(NULL))

    # If the starting coefficients are not good enough we turn to 'optim' for a better guess.
    if (is.null(r)) {
      cli::cli_alert(paste0("Convergence problems. Switching to fit_optim"))
      coef_start <- fit_optim(dat, fofo, coef_start)$par
      r <- tryCatch(switch(algorithm,
                           nlsLM = minpack.lm::nlsLM(fofo, data = dat, start = coef_start, control = list(maxiter = 1024)),
                           nls = nls(fofo, data = dat, start = coef_start, control = list(maxiter = 1000)),
                           nlsr = nlsr::nlsr(fofo, data = dat, start = coef_start)),
                    error = function(e) return(NULL))
    }

    # Despite our best attempts, convergence could not be achieved.
    if (is.null(r)) {
      cli::cli_abort("Could not fit the data with the selected input parameters")
    }

    attr(r, "log_trans") <- TRUE

  } else {
    attr(r, "log_trans") <- FALSE
  }

  return(r)
}
