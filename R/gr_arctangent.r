#' Arc-tangent equations for growth
#'
#' @description
#' \code{gr_arctangent} returns a character string containing the expression for
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
#' print(gr_arctangent("td"))
#' print(gr_arctangent("rate"))
#' print(gr_arctangent("ti"))
#'

gr_arctangent <- function(equation_type = "rate") {

  # Checks.
  equation_type <- tolower(equation_type)
  equation_type <- match.arg(equation_type, c("rate", "ti", "td"))


  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y*(atan(k*t+offset)/pi+0.5)",
              rate = "1/tdiff*(tan((y2/max_y-0.5)*pi)-tan((y1/max_y-0.5)*pi))",
              ti   = "max_y*(atan(tan((y1/max_y-0.5)*pi)+k*tdiff)/pi+0.5)"
  )

  return(z)
}
