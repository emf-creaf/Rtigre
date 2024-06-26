% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gr_arctangent.r
\name{gr_arctangent}
\alias{gr_arctangent}
\title{Arc-tangent equations for growth}
\usage{
gr_arctangent(equation_type = "rate")
}
\arguments{
\item{equation_type}{character. It indicates which equation will be
calculated, namely growth rate, time-independent or time-dependent growth.}
}
\value{
R expression of equation.
}
\description{
Arc-tangent equations for growth
}
\details{
See accompanying Vignette.
}
\examples{

print(gr_arctangent("td"))
print(gr_arctangent("rate"))
print(gr_arctangent("ti"))

}
