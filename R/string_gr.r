#' Title
#'
#' @param curve_type
#' @param equation_type
#'
#' @return
#' @export
#'
#' @examples
string_gr <- function(curve_type = "logistic", equation_type = "rate") {

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
