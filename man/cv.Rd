\name{cv}
\alias{cv}

\title{
Coefficient of variation
}
\description{
Calculating the coefficient of variation (cv), standardized and non-standardized
}
\usage{
cv(x, norm = FALSE)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}
  \item{norm}{
boolean argument that indicates if the function output is the standardized cv or not (default: \code{norm = FALSE})
}
}
\details{
The coefficient of variation \eqn{v} is a simple standardized measure of distribution and is used especially in the analysis of regional economic disparities (e.g. disparities in regional GDP per capita) or disparities in supply (e.g. density of physicians or grocery stores). The function returns the non-standardized cv (\eqn{0 < v < \infty}) or the standardized cv (\eqn{0 < v* < 1}).  
}
\value{
Single numeric value. If \code{norm = FALSE} the function returns the non-standardized cv (\eqn{0 < v < \infty}). If \code{norm = TRUE} the standardized cv (\eqn{0 < v* < 1}) is returned.
}
\references{
Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.cesifo-group.de/link/ifodb_2005_3_25-33.pdf}.

}
\author{
Thomas Wieland
}

\examples{
data(Goettingen)
# Loads the data
# (Grocery stores in Goettingen, Germany, at the level of 69 statistical districts:
# number and sales area of stores, absolute values and per 1.000 inhabitants)
cv(Goettingen$stores_p1000p)
# cv of stores per 1.000 inhabitants
cv(Goettingen$salesarea_p1000p)
# cv of sales area per 1.000 inhabitants
cv(Goettingen$salesarea_p1000p, norm=TRUE)
# standardized cv of sales area per 1.000 inhabitants
}
