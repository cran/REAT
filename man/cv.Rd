\name{cv}
\alias{cv}

\title{
Coefficient of variation
}
\description{
Calculating the coefficient of variation (cv), standardized and non-standardized
}
\usage{
cv(x, coefnorm = FALSE)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}
  \item{coefnorm}{
logical argument that indicates if the function output is the standardized cv or not (default: \code{coefnorm = FALSE})
}
}
\details{
The coefficient of variation \eqn{v} is a simple standardized measure of distribution and is used especially in the analysis of regional economic disparities (e.g. disparities in regional GDP per capita) or disparities in supply (e.g. density of physicians or grocery stores). The function returns the non-standardized cv (\eqn{0 < v < \infty}) or the standardized cv (\eqn{0 < v* < 1}).  
}
\value{
Single numeric value. If \code{coefnorm = FALSE} the function returns the non-standardized cv (\eqn{0 < v < \infty}). If \code{coefnorm = TRUE} the standardized cv (\eqn{0 < v* < 1}) is returned.
}
\references{
Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.cesifo-group.de/link/ifodb_2005_3_25-33.pdf}.

Huang, Y./Leung, Y. (2009): \dQuote{Measuring Regional Inequality: A Comparison of Coefficient of Variation and Hoover Concentration Index}. In: In: \emph{The Open Geography Journal}, \bold{2}, p. 25-34.
}
\author{
Thomas Wieland
}

\examples{
# Supply of grocery stores:
data(Goettingen)
# Loads the data
# (Grocery stores in Goettingen, Germany, at the level of 69 statistical districts:
# number and sales area of stores, absolute values and per 1.000 inhabitants)
cv(Goettingen$stores_p1000p)
# cv of stores per 1.000 inhabitants
cv(Goettingen$salesarea_p1000p)
# cv of sales area per 1.000 inhabitants
cv(Goettingen$salesarea_p1000p, coefnorm=TRUE)
# standardized cv of sales area per 1.000 inhabitants

# Supply of physicians:
data(health2)
# Demographic data for 420 small regions
data(health4)
# Medical supply in the 420 regions
supply_physgen <- merge(health4, health2)
supply_physgen$physgen_pop <- supply_physgen$phys_gen/supply_physgen$pop*1000
# Calculation: physicians per 1.000 inhabitants
supply_physgen[is.na(supply_physgen$physgen_pop),]$physgen_pop <- 0
cv(supply_physgen$physgen_pop)
# cv of physician supply
}
