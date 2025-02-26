#' Title
#'
#' @param dat
#' @param fo
#'
#' @returns
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
    expression <- parse(text = paste0("mean((", paste0(lhs, " - (", rhs, "))^2)")))
    eval(parse(text = expression), envir = dat)
  }


  # Call to "optim".
  out <- optim(par = coef, fn = fn, gr = NULL, dat, method = "Nelder-Mead")
  out <- optim(par = out$par, fn = fn, gr = NULL, dat, method = "Nelder-Mead")
  out <- optim(par = out$par, fn = fn, gr = NULL, dat, method = "BFGS")

  hess <- numDeriv::hessian(fn, out$par, method = "Richardson",
                               method.args = list(eps = 1e-4, d = 0.1,
                                                  zero.tol = sqrt(.Machine$double.eps/7e-7),
                                                  r = 4, v = 2, show.details = FALSE),
                               dat)

browser()
  return(list(coef = out$par, hessian = out$hessian))

}
