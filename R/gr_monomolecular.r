#' Monomolecular equations for growth
#'
#' @param equation_type character. It indicates which equation will be
#' calculated, namely growth rate, time-independent or time-dependent growth.
#'
#' @return R expression of equation.
#' @details See accompanying Vignette.
#' @export
#'
#' @examples
#'
#' print(gr_monomolecular("td"))
#' print(gr_monomolecular("rate"))
#' print(gr_monomolecular("ti"))
#'

gr_monomolecular <- function(equation_type = "rate") {

  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y*(1-exp(-(k*t+offset)))",
              rate = "1/tdiff*log((max_y-y1)/(max_y-y2))",
              ti   = "max_y-(max_y-y1)^exp(-k*tdiff)"
  )

  return(z)
}
