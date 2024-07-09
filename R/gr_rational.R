#' Rational equation for growth
#'
#' @description
#' \code{gr_rational} returns a character string containing the expression for
#' the selected equation type.
#'
#' @param equation_type character. It indicates which expression will be
#' returned, namely growth rate ("rate"), time-independent ("ti")
#' or time-dependent ("td") growth.
#'
#' @return R string containing the selected equation.
#'
#' @details See accompanying Vignette.
#'
#' @export
#'
#' @examples
#'
#' print(gr_rational("td"))
#' print(gr_rational("rate"))
#' print(gr_rational("ti"))
#'

gr_rational <- function(equation_type = "rate") {

  # Checks.
  equation_type <- tolower(equation_type)
  equation_type <- match.arg(equation_type, c("rate", "ti", "td"))


  # Equations are rather complicated.
  if (any(equation_type %in% c("rate", "ti"))) {
    f <- function(y) {
      q <- paste0("(2*", y, "/max_y-1)")
      paste0(q, "/sqrt(1-", q, "^2)")
    }
  }

  # Choice of equation type.
  z <- switch(equation_type,
              td   = "max_y/2*((k*t+offset)/sqrt(1+(k*t+offset)^2)+1)",
              rate = paste0("1/tdiff*(", f("y2"), "-", f("y1"), ")"),
              ti   = paste0("0.5*max_y*(1+(k*tdiff+", f("y1"), ")/sqrt(1+(k*tdiff+", f("y1"), ")^2))")
  )


  return(z)
}
