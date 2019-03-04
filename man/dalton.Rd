\name{dalton}
\alias{dalton}

\title{
Dalton Inequality Index
}

\description{
Calculating the Dalton Inequality Index e.g. with respect to regional income 
}

\usage{
dalton(x, na.rm = TRUE)
}

\arguments{
  \item{x}{
A \code{numeric} vector (e.g. dataset of regional income)
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
The \emph{Dalton Inequality Index} (\eqn{\delta}) can be used for economic inequality and/or regional disparities (Portnov/Felsenstein 2010).
}

\value{
A single numeric value of the \emph{Dalton Inequality Index}.
}

\references{
Portnov, B.A./Felsenstein, D. (2010): \dQuote{On the suitability of income inequality measures for regional analysis: Some evidence from simulation analysis and bootstrapping tests}. In: \emph{Socio-Economic Planning Sciences}, \bold{44}, 4, p. 212-219.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{cv}}, \code{\link{gini}}, \code{\link{gini2}}, \code{\link{herf}}, \code{\link{theil}}, \code{\link{hoover}}, \code{\link{coulter}}, \code{\link{dalton}}, \code{\link{disp}} 
}

\examples{
dalton (c(10,10,10,10))

dalton (c(10,0,0,0))

dalton (c(10,1,1,1))
}