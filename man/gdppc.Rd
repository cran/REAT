\name{gdppc}
\alias{gdppc}
\docType{data}
\title{
Gross Domestic Product (GDP) per capita for German counties 2000-2014
}
\description{
The dataset contains the Gross Domestic Product (GDP) per capita (in EUR, at current prices) for the 402 German counties (Landkreise) from 2000 to 2014.
}
\usage{data("gdppc")}
\format{
  A data frame with 402 observations on the following 24 variables.
  \describe{
    \item{\code{region_code_EU}}{a factor with levels 402 levels containing the EU code of the region}
    \item{\code{region_code}}{a factor with levels 402 levels containing the national code of the region}
    \item{\code{gdppc2000}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2000}
    \item{\code{gdppc2001}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2001}
    \item{\code{gdppc2002}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2002}
    \item{\code{gdppc2003}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2003}
    \item{\code{gdppc2004}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2004}
    \item{\code{gdppc2005}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2005}
    \item{\code{gdppc2006}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2006}
    \item{\code{gdppc2007}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2007}
    \item{\code{gdppc2008}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2008} 
    \item{\code{gdppc2009}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2009}
    \item{\code{gdppc2010}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2010}
    \item{\code{gdppc2011}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2011}
    \item{\code{gdppc2012}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2012}
    \item{\code{gdppc2013}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2013}
    \item{\code{gdppc2014}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 2014}
  }
}

\source{
Arbeitskreis "Volkswirtschaftliche Gesamtrechnungen der Laender" im Auftrag der Statistischen Aemter der 16 Bundeslaender, des Statistischen Bundesamtes und des Buergeramtes, Statistik und Wahlen, Frankfurt a. M. (2016): \dQuote{Bruttoinlandsprodukt, Bruttowertschoepfung in den kreisfreien Staedten und Landkreisen der Bundesrepublik Deutschland 1992 und 1994 bis 2014}. \url{https://www.destatis.de/DE/Publikationen/Thematisch/VolkswirtschaftlicheGesamtrechnungen/VGRderLaender/VGR_KreisergebnisseBand1_5820009147005.xlsx;jsessionid=67667AAEB4F8C8A23A70A5A2641C4D2C.cae3?__blob=publicationFile}
}

\references{
Arbeitskreis "Volkswirtschaftliche Gesamtrechnungen der Laender" im Auftrag der Statistischen Aemter der 16 Bundeslaender, des Statistischen Bundesamtes und des Buergeramtes, Statistik und Wahlen, Frankfurt a. M. (2016): \dQuote{Bruttoinlandsprodukt, Bruttowertschoepfung in den kreisfreien Staedten und Landkreisen der Bundesrepublik Deutschland 1992 und 1994 bis 2014}. \url{https://www.destatis.de/DE/Publikationen/Thematisch/VolkswirtschaftlicheGesamtrechnungen/VGRderLaender/VGR_KreisergebnisseBand1_5820009147005.xlsx;jsessionid=67667AAEB4F8C8A23A70A5A2641C4D2C.cae3?__blob=publicationFile}
}

\examples{
# Regional disparities / sigma convergence in Germany
data(gdppc)
# GDP per capita for German counties (Landkreise)
cvs <- apply (gdppc[3:17], MARGIN = 2, FUN = cv)
# Calculating cv for the years 2000-2014
years <- 2000:2014
plot(years, cvs, "l", ylim=c(0.3,0.6), xlab = "year", 
ylab = "CV of GDP per capita")
# Plot cv over time
}