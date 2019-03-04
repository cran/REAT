\name{coulter}
\alias{coulter}

\title{
Coulter Coefficient
}

\description{
Calculating the Coulter Coefficient e.g. with respect to regional income 
}

\usage{
coulter(x, weighting = NULL, na.rm = TRUE)
}

\arguments{
  \item{x}{
A \code{numeric} vector (e.g. dataset of regional income)
}
  \item{weighting}{
a weighting vector, e.g. population
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
The \emph{Coulter Coefficient} (\eqn{CC}) varies between 0 (no inequality/concentration) and 1 (complete inequality/concentration). It can be used for economic inequality and/or regional disparities (Portnov/Felsenstein 2010).    
}

\value{
A single numeric value of the \emph{Coulter Coefficient} (\eqn{0 < CC < 1}).
}

\references{
Portnov, B.A./Felsenstein, D. (2010): \dQuote{On the suitability of income inequality measures for regional analysis: Some evidence from simulation analysis and bootstrapping tests}. In: \emph{Socio-Economic Planning Sciences}, \bold{44}, 4, p. 212-219.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{cv}}, \code{\link{gini}}, \code{\link{gini2}}, \code{\link{herf}}, \code{\link{theil}}, \code{\link{hoover}}, \code{\link{atkinson}}, \code{\link{dalton}}, \code{\link{disp}} 
}

\examples{
bip <- c(400,400,400, 400, NA)
bev <- c(1,1,1,200, NA)
coulter(bip, bev)
}