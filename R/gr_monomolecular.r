#' Monomolecular equations for growth
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
#' print(gr_monomolecular("td"))
#' print(gr_monomolecular("rate"))
#' print(gr_monomolecular("ti"))
#'

gr_monomolecular <- function(equation_type = "rate") {

  # Checks.
  equation_type <- tolower(equation_type)
  equation_type <- match.arg(equation_type, c("rate", "ti", "td"))


  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y*(1-exp(-(k*t+offset)))",
              rate = "1/tdiff*log((max_y-y1)/(max_y-y2))",
              ti   = "max_y-(max_y-y1)^exp(-k*tdiff)"
  )

  return(z)
}
