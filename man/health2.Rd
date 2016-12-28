\name{health2}
\alias{health2}
\docType{data}
\title{
Subregions in South Lower Saxony
}
\description{
Dataset containing 420 subregions in South Lower Saxony and corresponding demographic data.
}
\usage{data("health2")}
\format{
  A data frame with 420 observations on the following 8 variables.
  \describe{
    \item{\code{region}}{Region ID}
    \item{\code{pop}}{a numeric vector containing the population of the subregion}
    \item{\code{lat}}{a numeric vector containing the latitude of the subregion}
    \item{\code{lon}}{a numeric vector containing the longitude of the subregion}
    \item{\code{city}}{a logical argument that indicates of the subregion is part of the city of Goettingen or not}
    \item{\code{share65}}{a numeric vector containing the share of inhabitants in the age of 65 or older}
    \item{\code{share18}}{a numeric vector containing the share of inhabitants younger than 18}
    \item{\code{asc}}{a numeric vector containing the corresponding age structure coefficient}
  }
}

\source{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\references{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\examples{
# Pharmacies in a 500 m buffer:
data(health1)
# Health service locations (physicians and pharmacies)
data(health2)
# 420 regions
health2_city <- health2[health2$city == 1,] 
# only regions in the city of Goettingen (city=1)
pharmacy <- health1[health1$type == "pharm",] 
# Only pharmacies
pharm_city <- dist.buf(health2_city, "region", "lat", "lon", pharmacy, 
"location", "lat", "lon", bufdist = 500)
# Pharmacies in a 500 m buffer from the statistical districts in Goettingen
}
