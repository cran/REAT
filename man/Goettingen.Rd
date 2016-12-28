\name{Goettingen}
\alias{Goettingen}
\docType{data}
\title{
Grocery store supply in Goettingen
}
\description{
Grocery stores and their sales areas in Goettingen (Germany) on the level of 69 statistical districts
}
\usage{data("Goettingen")}
\format{
  A data frame with 69 observations on the following 5 variables.
  \describe{
    \item{\code{statdist}}{a factor with 69 levels containing the codes of the statistical districts (\code{010}, \code{011}, ..., \code{180})}
    \item{\code{stores_count}}{a numeric vector containing the number of grocery stores in the regarded district}
    \item{\code{salesarea_sqm}}{a numeric vector containing the sales area (in sqm) of the grocery stores in the regarded district}
    \item{\code{stores_p1000p}}{a numeric vector containing the density of grocery stores in the regarded district (stores per 1.000 inhabitants)}
    \item{\code{salesarea_p1000p}}{a numeric vector containing the density of grocery sales area in the regarded district (sales area in sqm per 1.000 inhabitants)}
  }
}
\details{
The grocery store dataset contains all grocery stores (supermarkets and discounters) in Goettingen (Germany) as of march 2011. 
}
\source{
Wieland, T. (2011): \dQuote{Nahversorgung mit Lebensmitteln in Goettingen 2011 - Eine Analyse der Angebotssituation im Goettinger Lebensmitteleinzelhandel unter besonderer Beruecksichtigung der Versorgungsqualitaet}. \emph{Goettinger Statistik Aktuell}, \bold{35}. Goettingen. 
\url{http://www.goesis.goettingen.de/pdf/Aktuell35.pdf}.
}

\examples{
data(Goettingen)
# Loads the data
cv(Goettingen$stores_p1000p)
# cv of stores per 1.000 inhabitants
cv(Goettingen$salesarea_p1000p)
# cv of sales area per 1.000 inhabitants
cv(Goettingen$salesarea_p1000p, coefnorm = TRUE)
# standardized cv of sales area per 1.000 inhabitants
}