#' List of sigmoid functions available.
#'
#' @return
#' A list with names for all available sigmoid functions.
#'
#' @noRd
#'
#' @examples
#' all_curve_types()
#'
all_curve_types <- function() {
  c("logistic", "schumacher", "gompertz", "monomolecular", "monomolecular2", "arctangent",
    "arctangent_exp", "hyperbolic", "rational", "user")
}



#' Column names for each type of equation
#'
#' @param equation_type \code{character} string with three possible values: 'td', 'ti' or 'rate'.
#' This is not tested.
#'
#' @return
#' A character vector of length 4 containing the minimum set of column names that are required by
#' each type of equation.
#'
#' @noRd
#'
#' @examples
#' col_names("td")
#' col_names("rate")
#' col_names("ti")
col_names <- function(equation_type) {
  switch(equation_type,
         td   = c("t", "k", "offset", "max_y"),
         rate = c("y1", "y2", "tdiff", "max_y"),
         ti   = c("y1", "k", "tdiff", "max_y"))
}


tryC <- function(expr, verbose = F) {
  z <- tryCatch(expr,
#' Title
#'
#' @param w
#'
#' @return
#' @noRd
#'
#' @examples
                warning = function(w) {
                  if (verbose) cli::cli_warn("Calculation warning: {e$message}")
                  w
                }, error = function(e) {
                  if (verbose) cli::cli_alert("Calculation error: {e$message}")
                  e
                })
  if (verbose) {
    if (!inherits(z, c("warning", "error"))) {
      cli::cli_text("Calculation completed successfully!")
    }
  }

  return(z)
}
