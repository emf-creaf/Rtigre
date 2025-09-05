#' Predict method for nls2 objects
#'
#' Predicts from a fitted Rtigre model. If the 'log.transf' attribute is TRUE, the predictions are exponentiated.
#'
#' @param object An object of class "Rtigre".
#' @param newdata A data frame in which to look for variables with which to predict.
#' @param ... Not used.
#'
#' @details
#' To undo the log-transformation implemented in the linear regression we must exponentiate the predicted values
#' as follows:
#'
#' \deqn{
#'   \text{Predicted Y} = e^{\hat{y} + \frac{\hat{\sigma}^2}{2}}
#' }
#' where \eqn{\hat{y}} is the predicted value on the log scale and \eqn{\hat{\sigma}^2}
#' is the variance of the model residuals.
#'
#' @return A numeric vector of predictions.
#'
#' @export
predict.Rtigre <- function(object, newdata = NULL, ...) {

  # Call the original predict.nls method
  pred <- predict.nls(object, newdata = newdata, ...)


  # Check the 'log_transf' attribute. If it exists and is TRUE, undo the transformation.
  if (!is.null(attr(object, "log_transf")) && attr(object, "log_transf")) {
    # If 'log_transf' is TRUE, exponentiate the predictions.
    pred <- exp(pred + 0.5*sigma(object)^2)
  }

  return(pred)
}
