#' Arc-tangent equations for growth
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
#' print(gr_arctangent("td"))
#' print(gr_arctangent("rate"))
#' print(gr_arctangent("ti"))
#'

gr_arctangent <- function(equation_type = "rate") {

  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y*(atan(k*t+offset)/pi+0.5)",
              rate = "1/tdiff*(tan((y2/max_y-0.5)*pi)-tan((y1/max_y-0.5)*pi))",
              ti   = "max_y*(atan(tan((y1/max_y-0.5)*pi)+k*tdiff)/pi+0.5)"
  )

  return(z)
}
