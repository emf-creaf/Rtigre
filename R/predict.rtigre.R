#' Title
#'
#' @param object
#' @param newdata
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
#'
predict.rtigre <- function(object, newdata) {

  if (missing(newdata)) {
    stop("newdata must be provided")
  }

  eval(object$m$formula[[3]],
       c(as.list(object$m$getPars()), newdata),
       environment(object$m$formula))
}
