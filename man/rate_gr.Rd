% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rate_gr.r
\name{rate_gr}
\alias{rate_gr}
\title{Rate parameter values in time-independent growth equations.}
\usage{
rate_gr(dat, curve_type = curve_type)
}
\arguments{
\item{dat}{\code{data.frame} containing at least four columns named 'y1', 'y2', 'tdiff' and 'max_y'. See Details
and accompanying Vignettes for a description.}

\item{curve_type}{character string. It determines which growth curve to use: 'logistic' (default),
'schumacher', 'gompertz', 'monomolecular', 'arctangent', 'hyperbolic' or 'user'.}
}
\value{
value of rate parameter for each row in 'dat'.
}
\description{
This function calculates the rate parameter value of several time-independent growth equations from two size values
obtained at two different times.
}
\details{
Some of the growth equations are taken from Table 6.2 in
Burkhart and Tomé (2012).
Columns 'y1' and 'y2' in 'dat' correspond to the sizes of the individual at 't1' and 't2', with column 'tdiff'='t2'-'t1'.
Column 'max_y' correspond to maximum size attainable by the individual when time tends to infinite. This must be known a priori
and is not calculated by the function.
By 'growth rate' we refer to a constant parameter that determines the amount of growth at each time step. In the accompanying
Vignettes, that parameter is always identified with the letter \code{k}.
}
\examples{

# In the examples below we will simulate a time-dependent growth curve and
# then we will recover the input k-value.

## Common parameters.
tdiff <- 1
t <- seq(1,100,by=tdiff)
max_y <- 5.7
k <- .05
y <- max_y/(1+exp(-(k*t-2))) + rnorm(length(t))*.01
plot(t,y,xlab="Time",ylab="Size")

## Logistic growth.
dat <- data.frame(tdiff=tdiff,max_y=max_y)
hist(rate_gr(cbind(dat,y1=y[-length(t)],y2=y[-1]),
curve_type = "logistic"),breaks=20,
xlab="k parameter",main="")

# Next, we carry out calculations with actual Pinus uncinata data from
# the Spanish Forest Inventories.
data("Punci_IFN")

## Add time difference between second and third Inventory.
Punci_IFN$tdiff <- 10

## Show how growth rate decreases as trees get larger.
k <- rate_gr(Punci_IFN)
plot(Punci_IFN$y1,k,log="y",xlab="Tree diameter (cm)",ylab="Growth rate")

# Other plots.
plot(Punci_IFN$prec,k,log="y",xlab="Precipitation (mm)",ylab="Growth rate")
plot(Punci_IFN$temp,k,log="y",xlab="Temperature (ºC)",ylab="Growth rate")

}
\references{
Burkhart, Harold E., and Margarida Tomé. "Growth functions." In Modeling forest trees and stands,
pp. 111-130. Springer, Dordrecht, 2012.
}
