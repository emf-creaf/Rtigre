% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gr_logistic.r
\name{gr_logistic}
\alias{gr_logistic}
\title{Logistic equations for growth}
\usage{
gr_logistic(equation_type = "rate")
}
\arguments{
\item{equation_type}{character. It indicates which equation will be
calculated, namely growth rate, time-independent or time-dependent growth.}
}
\value{
R expression of equation.
}
\description{
Logistic equations for growth
}
\details{
See accompanying Vignette.
}
\examples{

print(gr_logistic("td"))
print(gr_logistic("rate"))
print(gr_logistic("ti"))

}
