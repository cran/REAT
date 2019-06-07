\name{growth}
\alias{growth}

\title{
Growth rates
}

\description{
This function calculates the growth from two input numeric vectors
}

\usage{
growth(val1, val2, growth.type = "growth",
output = "rate", rate.perc = FALSE, log.rate = FALSE, 
factor.mean = "mean", time.periods = NULL)
}

\arguments{
  \item{val1}{
First numeric vector (e.g. employment at time \eqn{t})
}
  \item{val2}{
Second numeric vector (e.g. employment at time \eqn{t}) or data frame for times \emph{t+1, t+2, t+3, ..., t+n}
}
  \item{growth.type}{
Type of growth value that has to be calculated (absolute values or growth rate)
}
  \item{output}{
Type of output in the case of several years: growth rate (default: \code{output = "rate"}) or annual values (\code{output = "annual"})
}
  \item{rate.perc}{
Logical argument that indicates whether growth rates are expressed in percent or not
}
  \item{log.rate}{
Logical argument that indicates whether growth rates are logged or not
}
  \item{factor.mean}{
If growth factors are returned: arithmetic mean (\code{factor.mean = "mean"}) or geometric mean (\code{factor.mean = "geom"})
}
  \item{time.periods}{
No. of regarded time periods (for average growth rates)
}
}

\value{
A numeric vector containing the growth rates in the same order as stated
}

\author{
Thomas Wieland
}

\examples{
# Example from Farhauer/Kroell (2013):
region_A_t <- c(90,20,10,60)
region_A_t1 <- c(100,40,10,55)
# data for region A (time t and t+1)
nation_X_t <- c(400,150,150,400)
nation_X_t1 <- c(440,210,135,480)
# data for the national economy (time t and t+1)
growth(region_A_t, region_A_t1)

data(Freiburg)
# Loads the data
growth(Freiburg$e_Freiburg2008, Freiburg$e_Freiburg2014, growth.type = "rate")
# Industry-specific growth rates for Freiburg 2008 to 2014
}