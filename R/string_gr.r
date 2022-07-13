#' Choice of growth curve
#'
#' @description Yields a character string for one of seven growth curves.
#' @param curve_type string. It indicates which curve type to provide. Possible
#' values are: "logistic", "schumacher", "gompertz", "monomolecular", "arctangent",
#' "hyperbolic" or "user".
#' @param equation_type
#'
#' @return a character string containing an expression for growth rate parameter,
#' age/time-independent or age/time-dependent growth functions. For a detailed
#' definition of those expressions, see accompanying vignette.
#' @export
#'
#' @examples
#' string_gr("hyperbolic", "td")
#'

string_gr <- function(curve_type = "logistic", equation_type = "rate") {

  curve_type <- tolower(curve_type)
  if (!any(curve_type == c("logistic","schumacher","gompertz","monomolecular","arctangent","hyperbolic","user")))
    stop("Wrong 'curve_type' value")
  equation_type <- tolower(equation_type)
  if (!any(equation_type==c("rate","ti","td"))) stop("Wrong 'equation_type' value")

  x <- switch(curve_type,
              logistic      = gr_logistic(equation_type),
              schumacher    = gr_schumacher(equation_type),
              gompertz      = gr_gompertz(equation_type),
              monomolecular = gr_monomolecular(equation_type),
              arctangent    = gr_arctangent(equation_type),
              hyperbolic    = gr_hyperbolic(equation_type),
              user          = gr_user(equation_type)
  )

  return(x)
}
