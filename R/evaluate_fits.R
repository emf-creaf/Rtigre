#' Title
#'
#' @param dat
#' @param fo
#' @param log_transf
#' @param train_proportion
#' @param num_simulations
#' @param positive_rate
#' @param k_param
#' @param algorithm
#'
#' @returns
#' @export
#'
#' @examples
evaluate_fits <- function(df, fo, log_transf = FALSE, train_proportion = 0.8, num_simulations = 100, positive_rate = FALSE, k_param = NULL, algorithm = "nlsLM") {


  # Checks. Most of them will be done by 'fit_growth'.
  stopifnot("Only values 0 < 'train_proportion' < 0 are allowed" = train_proportion>0 & train_proportion<1)
  stopifnot("'num_simulations' must be a positive integer" = num_simulations > 0 & as.integer(num_simulations) == num_simulations)


  # Simulation parameters.
  curves <- Rtigre:::all_curve_types()
  curves <- curves[-match("user", curves)]
  npoints <- nrow(df)
  ntrain <- round(npoints*.8)


  # Function to calculate performance indices.
  fperf <- function(x, y) c(cor(x, y)^2, mean(abs(x-y), na.rm = TRUE), sd(x-y, na.rm = TRUE))


  # Simulation loop.
  out <- array(0, dim = c(3, length(curves), num_simulations))
  for (i in 1:num_simulations) {
    j <- sample(npoints)[1:ntrain]
    df2 <- df[j, ]
    r <- lapply(curves, function(x) fit_growth(df2, fo, log_transf = log_transf, positive_rate = positive_rate, curve_type = x, verbose = FALSE))
    df2 <- df[-j, ]
    out[, , i] <- if (log_transf) {
      sapply(r, function(x) fperf(predict(x, newdata = df2), log(df2$y2-df2$y1)))
    } else {
      sapply(r, function(x) fperf(predict(x, newdata = df2), df2$y2-df2$y1))
    }
  }


  # Compute mean values and name rows and columns.
  out <- apply(out, c(1, 2), function(x) mean(x, na.rm = TRUE))
  rownames(out) <- c("R2", "MAE", "RMSE")
  colnames(out) <- curves


  return(out)



}
