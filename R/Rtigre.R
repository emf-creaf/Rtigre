#' Create a new Rtigre object
#'
#' Creates a new object of class "Rtigre" from an existing nls object.
#'
#' @param nls_obj An object of class "nls".
#' @param log_transf A logical value indicating whether predictions should be exponentiated.
#' @return An object of class "Rtigre".
#' @export
Rtigre <- function(nls_obj, log_transf = FALSE) {

  # Validate that the input is an nls object
  if (!inherits(nls_obj, "nls")) {
    stop("Input must be an 'nls' object.")
  }

  # Add the 'Rtigre' class and 'log_transf' attribute
  class(nls_obj) <- c("Rtigre", class(nls_obj))
  attr(nls_obj, "log_transf") <- log_transf

  return(nls_obj)
}
