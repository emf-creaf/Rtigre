#' Computation of growth curves
#'
#' @param dat data.frame. See
#' @param curve_type character string. It determines which curve to use: 'logistic' (default),
#' 'schumacher', 'gompertz', 'monomolecular', 'arctangent', 'hyperbolic' or 'user'.
#' @param equation_type character string. It can be equal to 'rate', 'td' or 'ti',
#' indicating whether to calculate growth rate parameter, time-dependent or
#' time-independent size, respectively.
#'
#' @return vector with length equal to the number of rows in \code{dat}, containing
#' either the growth rate parameter or size.
#'
#' @details The user is not supposed to call this function directly.
#' Use \code{\link{growth_rate}}, \code{\link{ti_size}} or \code{\link{td_size}} instead.
#'
#' @seealso [rate_transformation]
#'
#' @export
#'
#' @examples
#' dat <- data.frame(y1 = 10, y2 = 43, tdiff = 10, max_y = 80, t = 25, k = .025, offset = -.1)
#' eval_gr(dat, "logistic", "rate")
#' eval_gr(dat, "logistic", "td")
#' eval_gr(dat, "logistic", "ti")
#'
eval_gr <- function(dat, curve_type = "logistic", equation_type = "rate") {

  x <- with(dat, eval(parse(text = string_gr(curve_type, equation_type))))

  return(x)
}
