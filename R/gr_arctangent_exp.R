#' Arc-tangent exponential equations for growth
#'
#' @description
#' \code{gr_arctangent_exp} returns a character string containing the expression for
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
#' print(gr_arctangent_exp("td"))
#' print(gr_arctangent_exp("rate"))
#' print(gr_arctangent_exp("ti"))
#'

gr_arctangent_exp <- function(equation_type = "rate") {

  # Checks.
  equation_type <- tolower(equation_type)
  equation_type <- match.arg(equation_type, c("rate", "ti", "td"))


  # Chooses equation.
  z <- switch(equation_type,
              td   = "max_y*2/pi*atan(exp(k*t+offset))",
              rate = "1/tdiff*(log(tan(pi/(2*max_y)*y2))-log(tan(pi/(2*max_y)*y1)))",
              ti   = "2*max_y/pi*atan(exp(k*tdiff+log(tan(pi/(2*max_y)*y1))))"
  )

  return(z)
}
