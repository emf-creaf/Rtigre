#' Squared monomolecular equations for growth
#'
#' @description
#' \code{gr_monomolecular} returns a character string containing the expression for
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
#' print(gr_monomolecular2("td"))
#' print(gr_monomolecular2("rate"))
#' print(gr_monomolecular2("ti"))
#'

gr_monomolecular2 <- function(equation_type = "rate") {

  # Checks.
  equation_type <- tolower(equation_type)
  equation_type <- match.arg(equation_type, c("rate", "ti", "td"))


  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y*(1-exp(-(k*t+offset)^2))",
              rate = "1/tdiff*(sqrt(-log(1-y2/max_y))-sqrt(-log(1-y1/max_y)))",
              ti   = "max_y*(1-exp(-(k*tdiff+sqrt(-log(1-y1/max_y)))^2))"
  )

  return(z)
}
