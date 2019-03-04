\name{atkinson}
\alias{atkinson}

\title{
Atkinson Inequality Index
}

\description{
Calculating the Atkinson Inequality Index e.g. with respect to regional income 
}

\usage{
atkinson(x, epsilon = 0.5, na.rm = TRUE)
}

\arguments{
  \item{x}{
A \code{numeric} vector (e.g. dataset of regional income)
}
  \item{epsilon}{
A single value of the \eqn{\epsilon} weighting coefficient (default: \code{at.epsilon = 0.5})
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
The \emph{Atkinson Inequality Index} (\eqn{AI}) varies between 0 (no inequality/concentration) and 1 (complete inequality/concentration). It can be used for economic inequality and/or regional disparities (Portnov/Felsenstein 2010).    
}

\value{
A single numeric value of the \emph{Atkinson Inequality Index} (\eqn{0 < AI < 1}).
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
atkinson(c(100,0,0,0), epsilon = 0.8)

atkinson(c(100,100,100,100), epsilon = 0.8)
}