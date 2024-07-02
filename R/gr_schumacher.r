#' Schumacher equation for growth
#'
#' @description
#' \code{gr_schumacher} returns a character string containing the expression for
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
#' print(gr_schumacher("td"))
#' print(gr_schumacher("rate"))
#' print(gr_schumacher("ti"))
#'

gr_schumacher <- function(equation_type = "rate") {

  # Checks.
  equation_type <- tolower(equation_type)
  equation_type <- match.arg(equation_type, c("rate", "ti", "td"))


  # Choice of equation type.
  z <- switch(equation_type,
              td   = "max_y*exp(-1/(k*t+offset))",
              rate = "1/tdiff*(1/log(max_y/y2)-1/log(max_y/y1))",
              ti   = "max_y*exp(-(1/(1/log(max_y/y1)+k*tdiff)))"
  )

  return(z)
}
