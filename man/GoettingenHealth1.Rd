\name{GoettingenHealth1}
\alias{GoettingenHealth1}
\docType{data}

\title{
Healthcare providers in South Lower Saxony
}

\description{
Dataset with healthcare providers (general practitioners, psychotherapists, pharmacies) in two German counties (Goettingen and Northeim)
}

\usage{data("GoettingenHealth1")}

\format{
  A data frame with 617 observations on the following 5 variables.
  \describe{
    \item{\code{location}}{a numeric vector with unique IDs of the healthcare providers}
    \item{\code{lat}}{Latitude}
    \item{\code{lon}}{Longitude}
    \item{\code{type}}{Type of healthcare provider: general practitioners (phyh_gen), psychotherapists (psych) or pharmacies (pharm)}
    \item{\code{district}}{a numeric vector containing the IDs of the district the specific provider is located in}
  }
}

\source{
Wieland T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingn}. Research report, Georg-August-Universitaet Goeottingen, Geographisches Institut, Abteilung Humangeographie. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}.
}

\references{
Wieland T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingn}. Research report, Georg-August-Universitaet Goeottingen, Geographisches Institut, Abteilung Humangeographie. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}.
}

\examples{
\dontrun{
data(GoettingenHealth1)
# general practitioners, psychotherapists and pharmacies

area_goe <- 1753000000
# area of Landkreis Goettingen (sqm)
area_nom <- 1267000000
# area of Landkreis Northeim (sqm)
area_gn <- area_goe+area_nom
sqrt(area_gn/pi)

# this takes some seconds
ripley(GoettingenHealth1[GoettingenHealth1$type == "phys_gen",], 
"location", "lat", "lon", area = area_gn, t.max = 30000, t.sep = 300)

ripley(GoettingenHealth1[GoettingenHealth1$type == "pharm",], 
"location", "lat", "lon", area = area_gn, t.max = 30000, t.sep = 300)

ripley(GoettingenHealth1[GoettingenHealth1$type == "psych",], 
"location", "lat", "lon", area = area_gn, t.max = 30000, t.sep = 300)
}
}