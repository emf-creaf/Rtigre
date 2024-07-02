#' Computation of growth curves
#'
#' @description
#' Numerical evaluation of growth curves
#'
#' @param dat \code{data.frame} containing data to evaluate the curves.
#' @param curve_type character string. It determines which curve to use: 'logistic' (default),
#' 'schumacher', 'gompertz', 'monomolecular', 'arctangent', 'hyperbolic' or 'user'.
#' @param equation_type character string. It can be equal to 'td', 'rate' or 'ti',
#' indicating whether to calculate growth time-dependent size, rate parameter or
#' time-independent size, respectively.
#'
#' @return vector with length equal to the number of rows in \code{dat}, containing
#' either the growth rate parameter or size.
#'
#' @details
#' Depending on the value of the "equation_type" parameter the "dat" \code{data.frame}
#' must have the following columns: "t", "k", "offset" and "max_y" (equation_type = "td"),
#' "y1", "y2", "tdiff" and "max_y" (equation_type = "rate") and
#' "y1", "k", "tdiff" and "max_y" (equation_type = "ti").
#'
#' The user is not supposed to call this function directly.
#'
#' @export
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
  curve_type = match.arg(curve_type, c("logistic","schumacher","gompertz","monomolecular","arctangent","hyperbolic", "user"))
  equation_type <- match.arg(equation_type, c("td", "rate", "ti"))


  # Check column names.
  col_names <- switch(equation_type,
                      td   = c("t", "k", "offset", "max_y"),
                      rate = c("y1", "y2", "tdiff", "max_y"),
                      ti   = c("y1", "k", "tdiff", "max_y"))
  stopifnot("Wrong column names in 'dat'" = all(col_names %in% colnames(dat)))


  # Evaluate expression.
  x <- with(dat, eval(parse(text = string_gr(curve_type, equation_type))))

  return(x)
}
