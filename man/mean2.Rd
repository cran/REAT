\name{mean2}
\alias{mean2}

\title{
Calculation of mean (extended)
}
\description{
Calculating the arithmetic mean, weighted or non-weighted, or the geometric mean
}

\usage{
mean2(x, weighting = NULL, output = "mean", na.rm = FALSE)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}
  \item{weighting}{
a \code{numeric} vector containing weighting data to compute the weighted arithmetic mean (instead of the non-weighted)
}
  \item{output}{
argument to specify the output (\code{output = "mean"} returns the arithmetic mean, \code{output = "geom"} returns the geometric mean)
}
  \item{na.rm}{
logical argument that whether NA values should be extracted or not
}
}

\details{
This function uses the formula for the weighted arithmetic mean from Sheret (1984).
}

\value{
Single numeric value. If \code{output = "mean"} and \code{weighting} is specified, the function returns a weighted arithmetic mean. If \code{output = "geom"}, the geometric mean is returned.
}

\references{
Bahrenberg, G./Giese, E./Mevenkamp, N./Nipper, J. (2010): \dQuote{Statistische Methoden in der Geographie. Band 1: Univariate und bivariate Statistik}. Stuttgart: Borntraeger.

Sheret, M. (1984): \dQuote{The Coefficient of Variation: Weighting Considerations}. In: \emph{Social Indicators Research}, \bold{15}, 3, p. 289-295.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{sd2}}
}

\examples{
avector <- c(5, 17, 84, 55, 39)
mean(avector)
mean2(avector)
wvector <- c(9, 757, 44, 18, 682)
mean2 (avector, weighting = wvector)
mean2 (avector, output = "geom")
}