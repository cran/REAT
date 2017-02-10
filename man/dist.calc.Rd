\name{dist.calc}
\alias{dist.calc}

\title{
Euclidean distance between coordinates
}
\description{
Calculation of the euclidean distance between two points with stated coordinates (lat, lon)
}
\usage{
dist.calc(lat1, lon1, lat2, lon2, unit = "km")
}

\arguments{
  \item{lat1}{
Latitude of the regarded start point
}
  \item{lon1}{
Longitude of the regarded start point
}
  \item{lat2}{
Latitude of the regarded end point
}
  \item{lon2}{
Longitude of the regarded end point
}
  \item{unit}{
Unit of the resulting distance: \code{unit="m"} for meters, \code{unit="km"} for kilometers or \code{unit="miles"} for miles
}
}

\value{
A single numeric value
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{dist.buf}}, \code{\link{dist.mat}}
}
\examples{
dist.calc(51.556307, 9.947375, 49.009603, 8.417004)
# about 304 kilometers
}