#' Non-linear fit to a time-independent growth curve.
#'
#' @param dat
#' @param fo
#' @param type
#' @param sigmoid
#'
#' @return
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
#'
#' k <- 2/(1+exp(-(intercept+coef_temp*temp+coef_prec*prec)))+rnorm(length(temp))*.001
#' y1 <- max_y/(1+exp(-(k*t[1]-2)))
#' y2 <- max_y/(1+exp(-(k*t[2]-2)))
#' dat <- data.frame(tdiff=t[2]-t[1],max_y=max_y,y1=y1,y2=y2,temp=temp,prec=prec)
#'
#'
fit_growth <- function(dat, fo, curve_type = "logistic", sigmoid = T, kmax = NULL) {

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

  # Next, we build the formula.
  x <- paste0("(",x,")")
  y <- gr_logistic(equation_type = "ti")
  z <- gsub("k",x,y)
  fofo <- paste0("y2-y1~",z,"-y1")

  # The non-linear fit.
  r <- minpack.lm::nlsLM(formula(fofo), data = dat, start = coef_start, control=list(maxiter=1000))


browser()

}
