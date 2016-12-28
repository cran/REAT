\name{health1}
\alias{health1}
\docType{data}
\title{
Health service locations in South Lower Saxony
}

\description{
Dataset with health service locations in South Lower Saxony (General practitioners, pharmacies, psychotherapists, gynaecologists)
}

\usage{data("health1")}

\format{
  A data frame with 617 observations on the following 4 variables.
  \describe{
    \item{\code{location}}{Location ID}
    \item{\code{lat}}{Latitude of the location}
    \item{\code{lon}}{Longitude of the location}
    \item{\code{type}}{a factor with four levels containing the type of health service: \code{phys_gen} (general practitioners), \code{pharm} (pharmacies), \code{phys_gyn} (gynaecologists) and \code{psych} (psychotherapists)}
  }
}

\source{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\references{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\examples{
# Distances between geocoded pharmacies:
data(health1)
# Health service locations (physicians and pharmacies)
pharmacy <- health1[health1$type=="pharm",]
# Only pharmacies
pharmdistmat <- dist.mat (pharmacy[1:10,], "location", "lat", "lon", 
pharmacy, "location", "lat", "lon")
# Distance matrix for the first 10 pharmacy locations
}