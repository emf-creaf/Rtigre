#' Computation of growth curves
#'
#' @description
#' \code{eval_gr} Numerical evaluation of growth curves
#'
#' @param dat \code{data.frame} containing data to evaluate the curves.
#' @param curve_type \code{character} string. It determines which curve to use: 'logistic' (default),
#' 'schumacher', 'gompertz', 'monomolecular', 'monomolecular2', 'arctangent', 'hyperbolic', 'arctangent_exp', 'rational' or 'user'.
#' @param equation_type \code{character} string. It should be equal to 'td', 'rate' or 'ti',
#' indicating whether to calculate growth time-dependent size, rate parameter or
#' time-independent size, respectively.
#'
#' @return vector with length equal to the number of rows in \code{dat}, containing
#' either the growth rate parameter or size.
#'
#' @details
#' Depending on the value of the \code{equation_type} parameter \code{dat}
#' must have the following columns: "t", "k", "offset" and "max_y" (\code{equation_type = "td"}),
#' "y1", "y2", "tdiff" and "max_y" (\code{equation_type = "rate"}) and
#' "y1", "k", "tdiff" and "max_y" (\code{equation_type = "ti"}).
#'
#' Notice that subindices 1 and 2 refer to time points, so that
#' "y1" and "y2" are the quantities of interests at \eqn{t_{1}} and \eqn{t_{2}}, respectively.
#' See Vignettes for details.
#'
#' Although exported, the user is not supposed to call this function directly.
#'
#' @noRd
#'
#' @examples
#' dat <- data.frame(y1 = 10, y2 = 43, tdiff = 10, max_y = 80, t = 25, k = .025, offset = -.1)
#' eval_gr(dat, "logistic", "td")
#' eval_gr(dat, "logistic", "rate")
#' eval_gr(dat, "logistic", "ti")
#'
eval_gr <- function(dat, curve_type, equation_type) {

  # Checks.
  stopifnot("Input 'dat' must be a 'data.frame'" = is.data.frame(dat))
  curve_type = match.arg(curve_type, all_curve_types())
  equation_type <- match.arg(equation_type, c("td", "rate", "ti"))


  # Check column names.
  stopifnot("Wrong column names in 'dat'" = all(col_names(equation_type) %in% colnames(dat)))


  # Evaluate expression.
  x <- with(dat, eval(parse(text = string_gr(curve_type, equation_type))))


  return(x)
}
