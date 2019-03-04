\name{dist.buf}
\alias{dist.buf}

\title{
Counting points in a buffer
}
\description{
Counting points within a buffer of a given distance with points with given coordinates
}
\usage{
dist.buf(startpoints, sp_id, lat_start, lon_start, endpoints, ep_id, lat_end, lon_end, 
ep_sum = NULL, bufdist = 500, extract_local = TRUE, unit = "m")
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
A data frame containing the points to count
}
  \item{ep_id}{
Column containing the IDs of the points to count in the data frame \code{endpoints}
}
  \item{lat_end}{
Column containing the latitudes of the points to count in the data frame \code{endpoints}
}
  \item{lon_end}{
Column containing the longitudes of the points to count in the data frame \code{endpoints}
}
  \item{ep_sum}{
Column of an additional variable in the data frame \code{endpoints} to sum 
}
  \item{bufdist}{
The buffer distance
}
  \item{extract_local}{
Logical argument that indicates if the start points should be included or not (default: \code{TRUE})
}
  \item{unit}{
Unit of the buffer distance: \code{unit="m"} for meters, \code{unit="km"} for kilometers or \code{unit="miles"} for miles
}
}

\details{
The function is based on the idea of a buffer analysis in GIS (Geographic Information System), e.g. to count the points of interest within a given buffer distance.  
}

\value{
The function returns a \code{list} containing:
\item{count_table }{A \code{data.frame} containing two columns: The start point IDs (\code{from}) and the number of counted points in the given buffer distance (\code{count_location})}
\item{distmat }{A \code{data.frame} containing the corresponding distance matrix wiht \eqn{I x J} rows}
}

\references{
de Lange, N. (2013): \dQuote{Geoinformatik in Theorie und Praxis}. 3rd edition. Berlin : Springer Spektrum.

Krider, R. E./Putler, R. S. (2013): \dQuote{Which Birds of a Feather Flock Together? Clustering and Avoidance Patterns of Similar Retail Outlets}. In: \emph{Geographical Analysis}, \bold{45}, 2, p. 123-149}

\author{
Thomas Wieland
}

\seealso{
\code{\link{dist}}, \code{\link{dist.mat}}
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