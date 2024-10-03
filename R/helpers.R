all_curve_types <- function() {
  c("logistic", "schumacher", "gompertz", "monomolecular", "monomolecular2", "arctangent",
    "arctangent_exp", "hyperbolic", "rational", "user")
}

col_names <- function(equation_type) {
  switch(equation_type,
         td   = c("t", "k", "offset", "max_y"),
         rate = c("y1", "y2", "tdiff", "max_y"),
         ti   = c("y1", "k", "tdiff", "max_y"))
}


tryC <- function(expr, verbose = F) {
  z <- tryCatch(expr,
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
