\name{health3}
\alias{health3}
\docType{data}
\title{
Distance matrix for subregions in South Lower Saxony
}
\description{
Dataset containing a distance matrix for 420 subregions in South Lower Saxony (euclidean distance, travelling time)
}
\usage{data("health3")}
\format{
  A data frame with 176400 observations on the following 5 variables.
  \describe{
    \item{\code{from_to}}{a factor containing the interaction (from-to)}
    \item{\code{region_from}}{a numeric vector containing the ID of the start region}
    \item{\code{region_to}}{a numeric vector containing the ID of the end region}
    \item{\code{distance}}{a numeric vector containing the euclidean distance from \code{region_from} to \code{region_to}}
    \item{\code{drvtime}}{a numeric vector containing the travelling time from \code{region_from} to \code{region_to}}
  }
}

\source{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\references{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\examples{
data(health3)
health3_sample <- health3[sample(nrow(health3), 100), ]
# Random sample of 100 rows
plot(health3_sample$distance, health3_sample$drvtime)
# Plot of euclidean distance vs. travelling time
}