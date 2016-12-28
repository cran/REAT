\name{health4}
\alias{health4}
\docType{data}
\title{
Health service supply in South Lower Saxony subregions
}
\description{
Dataset containing the 420 subregions in South Lower Saxony (see \code{\link{health2}}) and the corresponding counts of health service locations (general practitioners, pharmacies, psychotherapists, gynaecologists, all physicians)
}
\usage{data("health4")}
\format{
  A data frame with 420 observations on the following 6 variables.
  \describe{
    \item{\code{region}}{a numeric vector containing the region ID}
    \item{\code{phys_gen}}{a numeric vector containing the number of general practitioners}
    \item{\code{phys_gyn}}{a numeric vector containing the number of gynaecologists}
    \item{\code{psych}}{a numeric vector containing the number of psychotherapists}
    \item{\code{phys_all}}{a numeric vector containing the number of all physicians}
    \item{\code{pharm}}{a numeric vector containing the number of pharmacies}
  }
}

\source{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\references{
Wieland, T./Dittrich, C. (2016): \dQuote{Bestands- und Erreichbarkeitsanalyse regionaler Gesundheitseinrichtungen in der Gesundheitsregion Goettingen}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2016/3-wieland.pdf}
}

\examples{
# Spatial concentration of health service locations and population:
data(health2)
# Population data
data(health4)
# Health service location data
gini(health2$pop)
# Gini coefficient for population
gini(health4$phys_gen)
# Gini coefficient for general practitioners
gini(health4$phys_gyn)
# Gini coefficient for gynaecologists
gini(health4$psych)
# Gini coefficient for psychotherapists
gini(health4$pharm)
# Gini coefficient for pharmacies
}