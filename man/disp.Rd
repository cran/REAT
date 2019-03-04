\name{disp}
\alias{disp}

\title{
Concentration/inequality/dispersion measures
}

\description{
Calculating a set of concentration/inequality/dispersion measures
}

\usage{
disp(x, weighting = NULL, at.epsilon = 0.5, na.rm = TRUE)
}

\arguments{
  \item{x}{
a \code{numeric} vector or matrix or columns from a data frame
}
  \item{weighting}{
a weighting vector, e.g. population
}
  \item{at.epsilon}{
Weighting parameter \eqn{\epsilon} for the Atkinson index
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
This function is a convenient wrapper for all functions calculating concentration/inequality measures.
}

\value{
A \code{matrix} containing the concentration/inequality measures.
}

\references{
Gluschenko, K. (2018): \dQuote{Measuring regional inequality: to weight or not to weight?} In: \emph{Spatial
Economic Analysis}, \bold{13}, 1, p. 36-59.

Portnov, B.A./Felsenstein, D. (2010): \dQuote{On the suitability of income inequality measures for regional analysis: Some evidence from simulation analysis and bootstrapping tests}. In: \emph{Socio-Economic Planning Sciences}, \bold{44}, 4, p. 212-219.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{atkinson}}, \code{\link{coulter}}, \code{\link{dalton}}, \code{\link{cv}}, \code{\link{gini2}}, \code{\link{herf}}, \code{\link{hoover}}, \code{\link{sd2}}, \code{\link{theil}}, \code{\link{williamson}} 
}

\examples{
data(Automotive)

disp(Automotive$Turnover2008)
disp(Automotive[4:8])
}