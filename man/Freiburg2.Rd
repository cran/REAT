\name{Freiburg2}
\alias{Freiburg2}
\docType{data}
\title{
Statistical districts of Freiburg
}
\description{
The 42 statistical districts of Freiburg (Germany) and the estimated annual purchasing power for groceries, based on average expenditures and population.
}
\usage{data("Freiburg2")}
\format{
  A data frame with 42 observations on the following 2 variables.
  \describe{
    \item{\code{district}}{a numeric vector representing the 42 statistical districts of Freiburg}
    \item{\code{ppower}}{a numeric vector containing the estimated absolute value of annual purchasing power for groceries in the district}
  }
}

\source{
Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}
}

\references{
Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}
}

\examples{
data(Freiburg1)
# Distance matrix for grocery stores in Freiburg
data(Freiburg2)
# Statistical districts of Freiburg
groceryfr <- huff (Freiburg1, "district", "store", "salesarea", "distance", gamma=1, lambda=-2)
# Huff interaction matrix for given grocery stores in Freiburg
# with standard weighting (power function with gamma=1 and lambda=-2)
groceryfr_total <- huff (Freiburg1, "district", "store", "salesarea", "distance", 
gamma=1, lambda=-2, localmarket_dataset = Freiburg2, origin_id = "district", 
localmarket = "ppower", output="total")
# Calculating total sales of the stores
}