\name{GoettingenHealth2}
\alias{GoettingenHealth2}
\docType{data}

\title{
Healthcare provision in South Lower Saxony
}

\description{
Dataset with districts in two German counties (Goettingen and Northeim) and the corresponding healthcare providers (general practitioners, psychotherapists, pharmacies) and population size
}

\usage{data("GoettingenHealth2")}

\format{
  A data frame with 420 observations on the following 7 variables.
  \describe{
    \item{\code{district}}{a numeric vector containing the IDs of the district}
    \item{\code{pop}}{no. of inhabitants}
    \item{\code{lat}}{Latitude}
    \item{\code{lon}}{Longitude}
    \item{\code{phys_gen}}{no. of general practitioners}
    \item{\code{psych}}{no. of psychotherapists}
    \item{\code{pharm}}{no. of pharmacies}
  }
}

\source{
Wieland T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingn}. Research report, Georg-August-Universitaet Goeottingen, Geographisches Institut, Abteilung Humangeographie. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}.
}

\references{
Wieland T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingn}. Research report, Georg-August-Universitaet Goeottingen, Geographisches Institut, Abteilung Humangeographie. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}.
}

\examples{
data(GoettingenHealth2)
# districts with healthcare providers and population size

williamson((GoettingenHealth2$phys_gen/GoettingenHealth2$pop), 
GoettingenHealth2$pop)
}