\name{theil}
\alias{theil}

\title{
Theil inequality index
}

\description{
Calculating the Theil inequality index
}

\usage{
theil(x, weighting = NULL, na.rm = TRUE)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}
  \item{weighting}{
a \code{numeric} weighting vector, e.g. population
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
Since there are several Theil measures of inequality, this function uses the formulation from Stoermann (2009).
}

\value{
A single numeric value of the \emph{Theil inequality index} (\eqn{0 < TI < 1}).
}

\references{
Portnov, B.A./Felsenstein, D. (2010): \dQuote{On the suitability of income inequality measures for regional analysis: Some evidence from simulation analysis and bootstrapping tests}. In: \emph{Socio-Economic Planning Sciences}, \bold{44}, 4, p. 212-219.

Stoermann, W. (2009): \dQuote{Regionaloekonomik: Theorie und Politik}. Muenchen : Oldenbourg.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{herf}}, \code{\link{hoover}} 
}

\examples{
# Example from Stoermann (2009):
regincome <- c(10,10,10,20,50)
theil(regincome) 
# 0.2326302
}