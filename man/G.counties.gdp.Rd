\name{G.counties.gdp}
\alias{G.counties.gdp}
\docType{data}
\title{
Gross Domestic Product (GDP) per capita for German counties 1992-2014
}
\description{
The dataset contains the Gross Domestic Product (GDP) absolute and per capita (in EUR, at current prices) for the 402 German counties (Landkreise) from 1992 to 2014.

}
\usage{data("G.counties.gdp")}
\format{
  A data frame with 402 observations on the following 68 variables.
  \describe{
    \item{\code{region_code_EU}}{a factor containing der EU regional code}
    \item{\code{region_code}}{a factor containing the German regional code}
    \item{\code{gdp1992}}{a numeric vector containing the GDP for German counties (Landkreise) for 1992}
    \item{\code{gdp1994}}{a numeric vector containing the GDP for German counties (Landkreise) for 1994}
    \item{\code{gdp1995}}{a numeric vector containing the GDP for German counties (Landkreise) for 1995}
    \item{\code{gdp1996}}{a numeric vector containing the GDP for German counties (Landkreise) for 1996}
    \item{\code{gdp1997}}{a numeric vector containing the GDP for German counties (Landkreise) for 1997}
    \item{\code{gdp1998}}{a numeric vector containing the GDP for German counties (Landkreise) for 1998}
    \item{\code{gdp1999}}{a numeric vector containing the GDP for German counties (Landkreise) for 1999}
    \item{\code{gdp2000}}{a numeric vector containing the GDP for German counties (Landkreise) for 2000}
    \item{\code{gdp2001}}{a numeric vector containing the GDP for German counties (Landkreise) for 2001}
    \item{\code{gdp2002}}{a numeric vector containing the GDP for German counties (Landkreise) for 2002}
    \item{\code{gdp2003}}{a numeric vector containing the GDP for German counties (Landkreise) for 2003}
    \item{\code{gdp2004}}{a numeric vector containing the GDP for German counties (Landkreise) for 2004}
    \item{\code{gdp2005}}{a numeric vector containing the GDP for German counties (Landkreise) for 2005}
    \item{\code{gdp2006}}{a numeric vector containing the GDP for German counties (Landkreise) for 2006}
    \item{\code{gdp2007}}{a numeric vector containing the GDP for German counties (Landkreise) for 2007}
    \item{\code{gdp2008}}{a numeric vector containing the GDP for German counties (Landkreise) for 2008}
    \item{\code{gdp2009}}{a numeric vector containing the GDP for German counties (Landkreise) for 2009}
    \item{\code{gdp2010}}{a numeric vector containing the GDP for German counties (Landkreise) for 2010}
    \item{\code{gdp2011}}{a numeric vector containing the GDP for German counties (Landkreise) for 2011}
    \item{\code{gdp2012}}{a numeric vector containing the GDP for German counties (Landkreise) for 2012}
    \item{\code{gdp2013}}{a numeric vector containing the GDP for German counties (Landkreise) for 2013}
    \item{\code{gdp2014}}{a numeric vector containing the GDP for German counties (Landkreise) for 2014}
    \item{\code{pop1992}}{a numeric vector containing the population for German counties (Landkreise) for 1992}
    \item{\code{pop1994}}{a numeric vector containing the population for German counties (Landkreise) for 1994}
    \item{\code{pop1995}}{a numeric vector containing the population for German counties (Landkreise) for 1995}
    \item{\code{pop1996}}{a numeric vector containing the population for German counties (Landkreise) for 1996}
    \item{\code{pop1997}}{a numeric vector containing the population for German counties (Landkreise) for 1997}
    \item{\code{pop1998}}{a numeric vector containing the population for German counties (Landkreise) for 1998}
    \item{\code{pop1999}}{a numeric vector containing the population for German counties (Landkreise) for 1999}
    \item{\code{pop2000}}{a numeric vector containing the population for German counties (Landkreise) for 2000}
    \item{\code{pop2001}}{a numeric vector containing the population for German counties (Landkreise) for 2001}
    \item{\code{pop2002}}{a numeric vector containing the population for German counties (Landkreise) for 2002}
    \item{\code{pop2003}}{a numeric vector containing the population for German counties (Landkreise) for 2003}
    \item{\code{pop2004}}{a numeric vector containing the population for German counties (Landkreise) for 2004}
    \item{\code{pop2005}}{a numeric vector containing the population for German counties (Landkreise) for 2005}
    \item{\code{pop2006}}{a numeric vector containing the population for German counties (Landkreise) for 2006}
    \item{\code{pop2007}}{a numeric vector containing the population for German counties (Landkreise) for 2007}
    \item{\code{pop2008}}{a numeric vector containing the population for German counties (Landkreise) for 2008}
    \item{\code{pop2009}}{a numeric vector containing the population for German counties (Landkreise) for 2009}
    \item{\code{pop2010}}{a numeric vector containing the population for German counties (Landkreise) for 2010}
    \item{\code{pop2011}}{a numeric vector containing the population for German counties (Landkreise) for 2011}
    \item{\code{pop2012}}{a numeric vector containing the population for German counties (Landkreise) for 2012}
    \item{\code{pop2013}}{a numeric vector containing the population for German counties (Landkreise) for 2013}
    \item{\code{pop2014}}{a numeric vector containing the population for German counties (Landkreise) for 2014}
    \item{\code{gdppc1992}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 1992}
    \item{\code{gdppc1994}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 1994}
    \item{\code{gdppc1995}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 1995}
    \item{\code{gdppc1996}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 1996}
    \item{\code{gdppc1997}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 1997}
    \item{\code{gdppc1998}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 1998}
    \item{\code{gdppc1999}}{a numeric vector containing the GDP per capita for German counties (Landkreise) for 1999}
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

\details{
For the years 1992 to 1999, the GDP data is incomplete.
}

\source{
Arbeitskreis "Volkswirtschaftliche Gesamtrechnungen der Laender" im Auftrag der Statistischen Aemter der 16 Bundeslaender, des Statistischen Bundesamtes und des Buergeramtes, Statistik und Wahlen, Frankfurt a. M. (2016): \dQuote{Bruttoinlandsprodukt, Bruttowertschoepfung in den kreisfreien Staedten und Landkreisen der Bundesrepublik Deutschland 1992 und 1994 bis 2014}. \url{https://www.destatis.de/DE/Publikationen/Thematisch/VolkswirtschaftlicheGesamtrechnungen/VGRderLaender/VGR_KreisergebnisseBand1_5820009147005.xlsx;jsessionid=67667AAEB4F8C8A23A70A5A2641C4D2C.cae3?__blob=publicationFile}
}

\references{
Arbeitskreis "Volkswirtschaftliche Gesamtrechnungen der Laender" im Auftrag der Statistischen Aemter der 16 Bundeslaender, des Statistischen Bundesamtes und des Buergeramtes, Statistik und Wahlen, Frankfurt a. M. (2016): \dQuote{Bruttoinlandsprodukt, Bruttowertschoepfung in den kreisfreien Staedten und Landkreisen der Bundesrepublik Deutschland 1992 und 1994 bis 2014}. \url{https://www.destatis.de/DE/Publikationen/Thematisch/VolkswirtschaftlicheGesamtrechnungen/VGRderLaender/VGR_KreisergebnisseBand1_5820009147005.xlsx;jsessionid=67667AAEB4F8C8A23A70A5A2641C4D2C.cae3?__blob=publicationFile}
}

\examples{
# Regional disparities / sigma convergence in Germany
data(G.counties.gdp)
# GDP per capita for German counties (Landkreise)
cvs <- apply (G.counties.gdp[54:68], MARGIN = 2, FUN = cv)
# Calculating cv for the years 2000-2014
years <- 2000:2014
plot(years, cvs, "l", ylim=c(0.3,0.6), xlab = "year", 
ylab = "CV of GDP per capita")
# Plot cv over time
}