\name{Freiburg1}
\alias{Freiburg1}
\docType{data}
\title{
Distance matrix for grocery stores in Freiburg
}
\description{
Preliminary stage of an interaction matrix: Distance matrix for all statistical 42 districts and all 63 grocery stores (\eqn{i} = 42 submarkets x \eqn{j} = 63 suppliers) in Freiburg (Germany) including the size of the grocery stores. 
}
\usage{data("Freiburg1")}
\format{
  A data frame with 2646 observations on the following 4 variables.
  \describe{
    \item{\code{district}}{a numeric vector representing the 42 statistical districts of Freiburg}
    \item{\code{store}}{a numeric vector identifying the store code of the mentioned grocery store in the study area}
    \item{\code{salesarea}}{a numeric vector for the sales area of the grocery stores in sqm}
    \item{\code{distance}}{a numeric vector for the distance from the places of residence (statistical districts) to the grocery stores in km}
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
data(Freiburg2)
# Loads the data

huff (Freiburg1, "district", "store", "salesarea", "distance")
# Huff probabilities with standard values (gamma = 1, lambda = -2)

huff (Freiburg1, "district", "store", "salesarea", "distance", gamma=1, lambda=-2, atype="pow", 
dtype="pow", gamma2 = NULL, lambda2=NULL, output_total = TRUE, Freiburg2, "ppower")
# Calculation of total sales using district data in Freiburg2
}