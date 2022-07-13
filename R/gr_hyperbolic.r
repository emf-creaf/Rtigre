#' Hyperbolic equations for growth
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
#' print(gr_hyperbolic("td"))
#' print(gr_hyperbolic("rate"))
#' print(gr_hyperbolic("ti"))
#'

gr_hyperbolic <- function(equation_type = "rate") {

  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y/(1+exp(-(2*k*t+offset)))",
              rate = "1/(2*tdiff)*log(y2/y1*(max_y-y1)/(max_y-y2))",
              ti   = "max_y/(1+exp(-2*k*tdiff)*(max_y/y1-1))"
  )

  return(z)
}
