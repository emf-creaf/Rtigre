#' Hyperbolic equations for growth
#'
#' @description
#' \code{gr_hyperbolic} returns a character string containing the expression for
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
#' print(gr_hyperbolic("td"))
#' print(gr_hyperbolic("rate"))
#' print(gr_hyperbolic("ti"))
#'

gr_hyperbolic <- function(equation_type = "rate") {

  # Checks.
  equation_type <- tolower(equation_type)
  equation_type <- match.arg(equation_type, c("rate", "ti", "td"))


  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y/(1+exp(-(2*k*t+offset)))",
              rate = "1/(2*tdiff)*log(y2/y1*(max_y-y1)/(max_y-y2))",
              ti   = "max_y/(1+exp(-2*k*tdiff)*(max_y/y1-1))"
  )

  return(z)
}
