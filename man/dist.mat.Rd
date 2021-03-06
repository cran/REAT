\name{dist.mat}
\alias{dist.mat}

\title{
Euclidean distance matrix between points
}
\description{
Calculation of an euclidean distance matrix between points with stated coordinates (lat, lon)
}
\usage{
dist.mat(startpoints, sp_id, lat_start, lon_start, endpoints, ep_id, 
lat_end, lon_end, unit = "km")
}

\arguments{
  \item{startpoints}{
A data frame containing the start points
}
  \item{sp_id}{
Column containing the IDs of the startpoints in the data frame \code{startpoints}
}
  \item{lat_start}{
Column containing the latitudes of the start points in the data frame \code{startpoints}
}
  \item{lon_start}{
Column containing the longitudes of the start points in the data frame \code{startpoints}
}
  \item{endpoints}{
A data frame containing the end points
}
  \item{ep_id}{
Column containing the IDs of the endpoints in the data frame \code{endpoints}
}
  \item{lat_end}{
Column containing the latitudes of the end points in the data frame \code{endpoints}
}
  \item{lon_end}{
Column containing the longitudes of the end points in the data frame \code{endpoints}
}
  \item{unit}{
Unit of the resulting distance: \code{unit="m"} for meters, \code{unit="km"} for kilometers or \code{unit="miles"} for miles
}
}

\details{
The function calculates an euclidean distance matrix between points with stated coordinates (lat and lon). While \eqn{m} start points and \eqn{n} end points are given, the output is a linear \eqn{m * n} distance matrix.  
}

\value{
The function returns a \code{data.frame} containing 4 columns: The start point IDs (\code{from}), the end point IDs (\code{to}), the combination of both (\code{from_to}) and the calculated distance (\code{distance}).
}

\references{
de Lange, N. (2013): \dQuote{Geoinformatik in Theorie und Praxis}. 3rd edition. Berlin : Springer Spektrum.

Krider, R. E./Putler, R. S. (2013): \dQuote{Which Birds of a Feather Flock Together? Clustering and Avoidance Patterns of Similar Retail Outlets}. In: \emph{Geographical Analysis}, \bold{45}, 2, p. 123-149}

\author{
Thomas Wieland
}

\seealso{
\code{\link{dist}}, \code{\link{dist.buf}}
}

\examples{
citynames <- c("Goettingen", "Karlsruhe", "Freiburg")
lat <- c(51.556307, 49.009603, 47.9874)
lon <- c(9.947375, 8.417004, 7.8945)
citynames <- c("Goettingen", "Karlsruhe", "Freiburg")
cities <- data.frame(citynames, lat, lon)
dist.mat (cities, "citynames", "lat", "lon", cities, "citynames", "lat", "lon")
# Euclidean distance matrix (3 x 3 cities = 9 distances)
dist.buf (cities, "citynames", "lat", "lon", cities, "citynames", "lat", "lon", bufdist = 300000)
# Cities within 300 km
}