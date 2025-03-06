#' Title
#'
#' @param dat data.frame
#' @param fo formula object
#' @param coef named numeric vector with initial values of the coefficients.
#' @param method character string corresponding to the \code{method} option in the \code{optim} function.
#'
#' @returns
#' A \code{nls} object.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = -10:100, x2 = runif(111), y = rnorm(111))
#' fo <- y ~ a * x1 + log(b * x2)
#' coef <- c(a = 2, b = .2)
#' fit_optim(dat, fo, coef)




fit_optim <- function(dat, fo, coef, method = "Nelder-Mead") {


  # Initial checks.
  stopifnot("Input 'dat' must be a data.frame" = is.data.frame(dat))
  stopifnot("Input 'fo' must be a formula" = inherits(fo, "formula"))
  stopifnot("Input 'coef' must be a named vector" = is.vector(coef))


  # Extract the left and right-hand sides
  rhs <- paste0(deparse(fo[[3]]), collapse = "")
  lhs <- paste0(deparse(fo[[2]]), collapse = "")


  # Function to minimize.
  fn <- function(coef, dat) {

    # Change 'dat' to list and add coefficients so that "eval" can have access to all variables.
    dat <- as.list(dat)
    for (i in names(coef)) dat[[i]] <- coef[i]

    # Evaluate and output.
    expression <- parse(text = paste0("sum((", paste0(lhs, " - (", rhs, "))^2, na.rm = TRUE)")))
    eval(parse(text = expression), envir = dat)
  }


  # Calls to "optim".
  out <- optim(par = coef, fn = fn, gr = NULL, dat, method = "Nelder-Mead")
  out <- optim(par = out$par, fn = fn, gr = NULL, dat, method = "Nelder-Mead")
  out <- optim(par = out$par, fn = fn, gr = NULL, dat, method = "BFGS")


  return(out)

}
