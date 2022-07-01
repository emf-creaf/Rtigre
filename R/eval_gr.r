#' Computation of growth curves
#'
#' @param dat data.frame. See
#' @param curve_type character string. It determines which curve to use: 'logistic' (default),
#' 'schumacher', 'gompertz', 'monomolecular', 'arctangent', 'hyperbolic' or 'user'.
#' @param equation_type character string. It can be equal to 'td', 'rate' or 'ti',
#' indicating whether to calculate growth time-dependent size, rate parameter or
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
#' eval_gr(dat, "logistic", "td")
#' eval_gr(dat, "logistic", "rate")
#' eval_gr(dat, "logistic", "ti")
#'
eval_gr <- function(dat, curve_type = "logistic", equation_type = "rate") {

  # Check inputs.
  col_names <- switch(equation_type,
                      td   = c("t","k","offset","max_y"),
                      rate = c("y1","y2","tdiff","max_y"),
                      ti   = c("y1","k","tdiff","max_y"))
  if (any(is.na(match(col_names,colnames(dat))))) stop("Wrong column names in 'dat'")
  if (!any(curve_type==c("logistic","schumacher","gompertz","monomolecular","arctangent","hyperbolic", "user")))
    stop("Wrong 'curve_type' value")
  if (!any(equation_type == c("td", "rate", "ti"))) stop("Wrong 'equation_type' value")

  x <- with(dat, eval(parse(text = string_gr(curve_type, equation_type))))

  return(x)
}
