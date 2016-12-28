\name{health5}
\alias{health5}
\docType{data}
\title{
Distance matrix for subregions and health service locations in South Lower Saxony
}
\description{
Dataset containing a distance matrix for 420 subregions and health service locations in South Lower Saxony (travelling time)
}
\usage{data("health5")}
\format{
  A data frame with 258523 observations on the following 3 variables.
  \describe{
    \item{\code{region_from}}{a numeric vector containing the ID of the start region}
    \item{\code{location_to}}{a numeric vector containing the ID of the location}
    \item{\code{drvtime}}{a numeric vector containing the travelling time from \code{region_from} to \code{location_to}}
  }
}

\source{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\references{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\examples{
# Hansen accessibility for psychotherapists in South Lower Saxony: 
data(health1)
# Health service locations in South Lower Saxony
data(health5)
# Distance matrix for health service locations and subregions in South Lower Saxony
psycho <- merge (health5, health1[health1$type == "psych",], 
by.x = "location_to", by.y = "location")
# Extracting psychotherapists
psycho <- psycho[sample(nrow(psycho), 10), ]
# Random sample of 10 rows (psychotherapists)
hansen_psych <- hansen(psycho, "region_from", "location_to", attrac = 1, 
"drvtime", lambda = -0.11, dtype = "exp", dist_const = 5)
# Hansen accessibility for psychotherapists with an exponential distance decay function
# with a weighting of lambda = -0.11 
}