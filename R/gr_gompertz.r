#' Gompertz equations for growth
#'
#' @description
#' \code{gr_gompertz} returns a character string containing the expression for
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
#' print(gr_gompertz("td"))
#' print(gr_gompertz("rate"))
#' print(gr_gompertz("ti"))
#'

gr_gompertz <- function(equation_type = "rate") {

  # Checks.
  equation_type <- tolower(equation_type)
  equation_type <- match.arg(equation_type, c("rate", "ti", "td"))


  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y*exp(-exp(-(k*t+offset)))",
              rate = "1/tdiff*log(log(max_y/y1)/log(max_y/y2))",
              ti   = "max_y*(y1/max_y)^exp(-k*tdiff)"
  )

  return(z)
}
