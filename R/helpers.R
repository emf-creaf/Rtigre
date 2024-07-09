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
