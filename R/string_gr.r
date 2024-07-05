#' Choice of growth curve
#'
#' @description
#' \code{string_gr} returns a character string for the selected curve type.
#'
#' @param curve_type string. It indicates which curve type to provide. Possible
#' values are: "logistic", "schumacher", "gompertz", "monomolecular", "arctangent",
#' "hyperbolic", "arctangent_exp", "rational" or "user".
#' @param equation_type string, can be equal to "rate", "ti" or "td". See Details.
#'
#' @return a character string containing an expression for the growth rate parameter
#' (\code{equation_type="rate"}),
#' age/time-independent (\code{equation_type="ti"})
#' or age/time-dependent (\code{equation_type="td"}) growth functions.
#' For a detailed definition of those expressions, see accompanying vignette.
#'
#' @export
#'
#' @examples
#' string_gr("hyperbolic", "td")
#'

string_gr <- function(curve_type = "logistic", equation_type = "rate") {


  # Checks.
  curve_type <- tolower(curve_type)
  stopifnot("Wrong 'curve_type'" = any(curve_type %in% c("logistic", "schumacher", "gompertz",
                                                         "monomolecular", "arctangent",
                                                         "hyperbolic", "arctangent_exp",
                                                         "rational", "user")))
  equation_type <- tolower(equation_type)
  equation_type <- match.arg(equation_type, c("rate", "ti", "td"))


  # Equation type.
  x <- switch(curve_type,
              logistic        = gr_logistic(equation_type),
              schumacher      = gr_schumacher(equation_type),
              gompertz        = gr_gompertz(equation_type),
              monomolecular   = gr_monomolecular(equation_type),
              arctangent      = gr_arctangent(equation_type),
              hyperbolic      = gr_hyperbolic(equation_type),
              arctangent_exp  = gr_arctangent_exp(equation_type),
              rational     = gr_rational(equation_type),
              user            = gr_user(equation_type)
  )


  return(x)
}
