#' Gompertz equations for growth
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
#' print(gr_gompertz("td"))
#' print(gr_gompertz("rate"))
#' print(gr_gompertz("ti"))
#'

gr_gompertz <- function(equation_type = "rate") {

  if (!any(equation_type==c("rate","ti","td")))
    stop("Wrong 'equation_type' value")

  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y*exp(-exp(-(k*t+offset)))",
              rate = "1/tdiff*log(log(max_y/y1)/log(max_y/y2))",
              ti   = "max_y*(y1/max_y)^exp(-k*tdiff)"
  )

  return(z)
}
