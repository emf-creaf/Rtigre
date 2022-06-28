#' Equations for logistic growth
#'
#' @param equation_type character. It yields the equation corresponding to growth rate,
#' time-independent growth or the usual time-dependent logistic curve.
#'
#' @return character string with equation.
#' @export
#'
#' @examples
#' print(gr_logistic(equation_type = "independent"))
gr_logistic <- function(equation_type = "rate") {

  if (!any(equation_type==c("rate","ti","td")))
    stop("Wrong 'equation_type' value")

  # Chooses equation.
  z <- switch(equation_type,
              rate = "1/tdiff*log(y2/y1*(max_y-y1)/(max_y-y2))",
              ti = "max_y/(1+exp(-k*tdiff)*(max_y/y1-1))",
              td = "max_y/(1+exp(-(k*t+offset)))"
  )

  return(z)
}
