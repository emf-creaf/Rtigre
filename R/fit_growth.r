#' Non-linear fit to a time-independent growth curve.
#'
#' @param dat data.frame containing data to be used in the fit.
#' @param fo formula describing the right-hand-side of the dependence of the
#' growth rate on the predictors.
#' @param sigmoid logical. If TRUE, growth rate will be a sigmoid (i.e. logistic).
#' @param curve_type character. It indicates the type of growth curve to be used.
#' @param kmax NULL or numeric. If NULL, its value will be calculated from the
#' data.
#' @param log_transf logical. If TRUE, a log-transformation will be applied to
#' the diameter increment.
#'
#' @return a \code{nls} object. See ?minpack.lm::nlsLM for details.
#'
#' @export
#'
#' @examples
#'
#' ## Common parameters. Simple example.
#' tdiff <- 5
#' t <- seq(1,100,by=tdiff)
#' max_y <- 120
#' k <- .1
#'
#' ## Fake climatic data.
#' temp <- runif(100,18.6,21.3)
#' prec <- runif(100,359,514)
#' t <- c(10,20)
#' intercept <- .02
#' coef_temp <- .00061
#' coef_prec <- .000052
#' k <- intercept+coef_temp*temp+coef_prec*prec+rnorm(length(temp))*.001
#' y1 <- max_y/(1+exp(-(k*t[1]-2)))
#' y2 <- max_y/(1+exp(-(k*t[2]-2)))
#' dat <- data.frame(tdiff=t[2]-t[1],max_y=max_y,y1=y1,y2=y2,temp=temp,prec=prec)
#'
#'
#' ## Same data, but simulating a sigmoid rate.
#' k <- 2/(1+exp(-(intercept+coef_temp*temp+coef_prec*prec)))+rnorm(length(temp))*.001
#' y1 <- max_y/(1+exp(-(k*t[1]-2)))
#' y2 <- max_y/(1+exp(-(k*t[2]-2)))
#' dat <- data.frame(tdiff=t[2]-t[1],max_y=max_y,y1=y1,y2=y2,temp=temp,prec=prec)
#'
#'
#' ## Actual Pinus uncinata data from the Spanish Forest Inventories.
#' data("Punci_IFN")
#'
#' ## Add time difference between second and third Inventory.
#' Punci_IFN$tdiff <- 10
#'
#' r <- fit_growth(Punci_IFN, ~prec+temp)
#' summary(r)
#'
#'
fit_growth <- function(dat, fo, curve_type = "logistic", sigmoid = T, kmax = NULL, log_transf = T) {

  cl <- match.call()
  m <- match(c("dat","fo"),names(cl))
  if (any(is.na(m))) stop("Missing argument")

  tf <- terms.formula(fo)
  is.intercept <- attr(tf,"intercept")
  if (length(is.intercept)==0) stop("Expression in formula 'fo' must have an intercept term")

  if (!any(curve_type==c("logistic","schumacher","gompertz","monomolecular","arctangent","hyperbolic")))
    stop("Wrong 'curve_type' value")

  # Get regression parameters.
  r <- fit_rate(dat = dat, fo = fo, curve_type = curve_type, sigmoid = sigmoid, kmax = kmax)
  coef_start <- coef(r)

  # Intercept term.
  x <- names(coef_start)[1]
  names_start <- "Intercept"

  # If fo contains more predictors, add them to the formula string.
  if (length(coef_start) > 1) {
    for (i in 2:length(coef_start)) {
      x <- paste0(x,"+coef_",names(coef_start)[i],"*",names(coef_start)[i])
      names_start <- c(names_start,paste0("coef_",names(coef_start)[i]))
    }
  }
  names(coef_start) <- names_start

  # If we opted for a sigmoid formula.
  if (sigmoid) {
    x <- paste0("kmax/(1+exp(-(",x,")))")
    coef_start <- c(r$kmax, coef_start)
    names(coef_start)[1] <- "kmax"
  }

  # Next, we build the formula. We leave it as a string in case a log-transformation
  # is required below. It's easy to work with strings in that case.
  x <- paste0("(",x,")")
  y <- string_gr(curve_type, "ti")
  z <- gsub("k",x,y)
  fofo <- paste0("y2-y1~",z,"-y1")

  # The non-linear fit.
  r <- minpack.lm::nlsLM(formula(fofo), data = dat, start = coef_start, control=list(maxiter=1000))

  # If a log-transformed regression is sought.
  if (log_transf) {
    fofo <- paste0("log(y2-y1)~log(",z,"-y1)")
    r <- minpack.lm::nlsLM(formula(fofo), data = dat, start = coef(r), control=list(maxiter=1000))
  }

  return(r)
}
