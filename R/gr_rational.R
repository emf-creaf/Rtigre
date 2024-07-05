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


  # Choice of equation type.
  z <- switch(equation_type,
              td   = "max_y*(k*t+offset)/sqrt(1+(k*t+offset)^2)",
              rate = "1/tdiff*(1/sqrt((max_y/y2)^2-1)-1/sqrt((max_y/y1)^2-1))",
              ti   = "max_y/sqrt(1+1/(k*tdiff+1/sqrt((max_y/y1)^2-1))^2)"
  )


  return(z)
}
