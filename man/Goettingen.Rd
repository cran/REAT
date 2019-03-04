\name{Goettingen}
\alias{Goettingen}
\docType{data}

\title{
Employment data for Goettingen and Germany 2008-2017
}

\description{
This dataset contains the employees in 15 economic sections (German Classification of Economic Activities WZ2008) for the city Goettingen and Germany regarding the years 2008-2017 (date: 30 June each year).  
}

\usage{data("Goettingen")}

\format{
  A data frame with 16 observations on the following 22 variables.
  \describe{
\item{\code{WZ2008_Code}}{a factor containing the code of the industry (15 economic sections from the German Classification of Economic Activities WZ2008 + total employees), in German language}
    \item{\code{WZ2008_Name}}{a factor containing the name of the industry (15 economic sections from the German Classification of Economic Activities WZ2008 + total employees), in German language}
    \item{\code{Goettingen2008}}{industry employees in the city of Goettingen 2008}
    \item{\code{Goettingen2009}}{industry employees in the city of Goettingen 2009}
    \item{\code{Goettingen2010}}{industry employees in the city of Goettingen 2010}
    \item{\code{Goettingen2011}}{industry employees in the city of Goettingen 2011}
    \item{\code{Goettingen2012}}{industry employees in the city of Goettingen 2012}
    \item{\code{Goettingen2013}}{industry employees in the city of Goettingen 2013}
    \item{\code{Goettingen2014}}{industry employees in the city of Goettingen 2014}
    \item{\code{Goettingen2015}}{industry employees in the city of Goettingen 2015}
    \item{\code{Goettingen2016}}{industry employees in the city of Goettingen 2016}
    \item{\code{Goettingen2017}}{industry employees in the city of Goettingen 2017}
    \item{\code{BRD2008}}{industry employees in Germany 2008}
    \item{\code{BRD2009}}{industry employees in Germany 2009}
    \item{\code{BRD2010}}{industry employees in Germany 2010}
    \item{\code{BRD2011}}{industry employees in Germany 2011}
    \item{\code{BRD2012}}{industry employees in Germany 2012}
    \item{\code{BRD2013}}{industry employees in Germany 2013}
    \item{\code{BRD2014}}{industry employees in Germany 2014}
    \item{\code{BRD2015}}{industry employees in Germany 2015}
    \item{\code{BRD2016}}{industry employees in Germany 2016}
    \item{\code{BRD2017}}{industry employees in Germany 2017}
  }
}

\source{
Bundesagentur fuer Arbeit (2018): \dQuote{Beschaeftigungsstatistik, Beschaeftigte nach Wirtschaftszweigen (WZ 2008) (Zeitreihe Quartalszahlen) in Deutschland}. \url{http://statistik.arbeitsagentur.de/Navigation/Statistik/Statistik-nach-Themen/Beschaeftigung/Beschaeftigung-Nav.html} (accessed October 10, 2018). Own postprocessing (filtering and aggregation).

Stadt Goettingen - Referat Statistik und Wahlen (2008): \dQuote{Stadt Goettingen: Beschaeftigte nach Wirtschaftsbereichen und Wirtschaftsabschnitten 1980 bis 2017. Table: IS 071.20}. \url{http://www.goesis.goettingen.de/pdf/071_20.pdf} (accessed October 8, 2018).
}

\references{
Bundesagentur fuer Arbeit (2018): \dQuote{Beschaeftigungsstatistik, Beschaeftigte nach Wirtschaftszweigen (WZ 2008) (Zeitreihe Quartalszahlen) in Deutschland}. \url{http://statistik.arbeitsagentur.de/Navigation/Statistik/Statistik-nach-Themen/Beschaeftigung/Beschaeftigung-Nav.html} (accessed October 10, 2018).

Federal Statistical Office Germany (2008): \dQuote{Classification of Economic Activities, Edition 2008 (WZ 2008)}. \url{https://www.destatis.de/DE/Methoden/Klassifikationen/GueterWirtschaftklassifikationen/klassifikationWZ08englisch.xls} (accessed October 10, 2018).

Stadt Goettingen - Referat Statistik und Wahlen (2008): \dQuote{Stadt Goettingen: Beschaeftigte nach Wirtschaftsbereichen und Wirtschaftsabschnitten 1980 bis 2017. Table: IS 071.20}. \url{http://www.goesis.goettingen.de/pdf/071_20.pdf} (accessed October 8, 2018).
}

\examples{
data(Goettingen)

# Location quotients for Goettingen 2017:
locq (Goettingen$Goettingen2017[2:16], Goettingen$Goettingen2017[1], 
Goettingen$BRD2017[2:16], Goettingen$BRD2017[1])

# Gini coefficient of regional specialization 2017:
gini.spec(Goettingen$Goettingen2017[2:16], Goettingen$BRD2017[2:16])

# Krugman coefficient of regional specialization 2017:
krugman.spec(Goettingen$Goettingen2017[2:16], Goettingen$BRD2017[2:16])
}