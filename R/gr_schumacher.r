#' Schumacher equations for growth
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
#' print(gr_schumacher("ti"))
#'

gr_schumacher <- function(equation_type = "rate") {

  if (!any(equation_type==c("rate","ti","td")))
    stop("Wrong 'equation_type' value")

  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y*exp(-1/(k*t+offset))",
              rate = "1/tdiff*(1/log(max_y/y2)-1/log(max_y/y1))",
              ti   = "max_y*exp(-(1/(1/log(max_y/y1)+k*tdiff)))"
  )

  return(z)
}
