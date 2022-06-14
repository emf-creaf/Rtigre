#' Regression of rate parameter
#'
#' This function computes a regression of the rate parameters against a set of predictor variables.
#' obtained at two different times.
#'
#' @param dat \code{data.frame} containing at least four columns named 'y1', 'y2', 'tdiff' and 'max_y'. See Details
#' and accompanying Vignettes for a description.
#' @param fo an object of class "formula"
#' @param type string to select which growth function to use. It can be equal to 'logistic',
#' 'schumacher', 'monomolecular', 'gompertz', 'arctangent' or 'hyperbolic'.
#' @param sigmoid logical; if set to TRUE
#'
#' @details Some of the growth equations are taken from Table 6.2 in
#' Burkhart and Tomé (2012).
#' Columns 'y1' and 'y2' in 'dat' correspond to the sizes of the individual at 't1' and 't2', with column 'tdiff'='t2'-'t1'.
#' Column 'max_y' correspond to maximum size attainable by the individual when time tends to infinite.
#'
#' @return value of rate parameter for each row in 'dat'.
#'
#' @examples
#'
#' # In the examples below we will calculate a time-dependent growth curve and will
#' # recover the input k-value.
#'
#' ## Common parameters.
#' tdiff <- 1
#' t <- seq(1,100,by=tdiff)
#' max_y <- 5.7
#' k <- .05
#' y <- max_y/(1+10*exp(-k*t))+runif(length(t))*.01
#' temp <- runif(100)
#' prec <- runif(100)
#' dat <- data.frame(tdiff=tdiff,max_y=max_y,y1=y[-length(t)],y2=y[-1],temp=temp[1:99],prec=prec[1:99])
#'
#' ## Logistic growth.
#' fo <- ~temp+prec
#' r <- rate_parameter_reg(dat,fo)
#'
#' k <- .03+.1*temp-.3*prec
#' y <- max_y/(1+10*exp(-k*t))+runif(length(t))*.01
#' dat$y1 <- y[-length(t)]
#' dat$y2 <- y[-1]
#' r <- rate_parameter_reg(dat,fo)
#'
#' @references
#' Burkhart, Harold E., and Margarida Tomé. "Growth functions." In Modeling forest trees and stands,
#' pp. 111-130. Springer, Dordrecht, 2012.
#'
#' @export

rate_parameter_reg <- function(dat, fo, type = "logistic", sigmoid = T) {

  cl <- match.call()
  m <- match(c("dat","fo"),names(cl))
  if (any(is.na(m))) stop("Missing argument")

  tf <- terms.formula(fo)
  is.intercept <- attr(tf,"intercept")
  is.terms <- attr(tf,"term.labels")
  if (length(is.intercept)==0) stop("Expression in formula 'fo' must have an intercept term")
  if (length(is.terms)==0) stop("Expression in formula 'fo' must have a predictor term")

  # Transformation to build a linear expression for k.
  dat <- cbind(dat,k=rate_parameter(dat, type = type))
  r <- lm(update(fo,k~.),data=dat)

  # sigmoid = TRUE only makes sense if there are predictor variables in the formula.
  if (sigmoid) {
    coef_r <- coef(r)
    kmax <- max(dat$k)
    k0 <- 4*coef_r[1]/kmax-2
    coef_names <- c("kmax","Intercept",names(coef_r[-1]))
    ex <- paste0("(Intercept+",paste("coef_",coef_names[-c(1,2)],"*",coef_names[-c(1,2)],sep="",collapse="+"),")")
    fof <- paste0("k~","kmax/(1+exp(-",ex,"))")

    coef_start <- c(kmax,k0,coef_r[-1]*4/kmax)
    names(coef_start) <- c("kmax","Intercept",paste0("coef_",coef_names[-c(1,2)]))
    fof <- paste0("k~","kmax/(1+exp(-",ex,"))")
    r <- minpack.lm::nlsLM(fof,data=dat,start=coef_start,control=list(maxiter=1000))
  }

  return(r)
}

