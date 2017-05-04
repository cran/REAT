\name{hoover}
\alias{hoover}

\title{
Hoover Concentration Index
}
\description{
Calculating the Hoover Concentration Index with respect to regional income (e.g. GDP) and population
}
\usage{
hoover(x, weighting = NULL)
}

\arguments{
  \item{x}{
A \code{numeric} vector (dataset of regional income, e.g. GDP)
}
  \item{weighting}{
A \code{numeric} weighting vector (dataset of regional population). If \code{weighting = NULL}, the shares of income are compared with the shares of regions (\eqn{1/n})
}
}

\details{
The \emph{Hoover Concentration Index} (\eqn{CI}) measures the economic concentration of income across space by comparing the share of income (e.g. GDP - Gross Domestic Product) with the share of population. The index varies between 0 (no inequality/concentration) and 1 (complete inequality/concentration). It can be used for economic inequality and/or regional disparities (Huang/Leung 2009).    
}

\value{
A single numeric value of the \emph{Hoover Concentration Index} (\eqn{0 < CI < 1}).
}

\references{
Bahrenberg, G./Giese, E./Mevenkamp, N./Nipper, J. (2010): \dQuote{Statistische Methoden in der Geographie. Band 1: Univariate und bivariate Statistik}. Stuttgart: Borntraeger.

Huang, Y./Leung, Y. (2009): \dQuote{Measuring Regional Inequality: A Comparison of Coefficient of Variation and Hoover Concentration Index}. In: In: \emph{The Open Geography Journal}, \bold{2}, p. 25-34.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{cv}}, \code{\link{gini}}, \code{\link{herf}}
}

\examples{
# Regional disparities in Germany:
gdp <- c(460.69, 549.19, 124.16, 65.29, 31.59, 109.27, 263.44, 39.87, 258.53, 
645.59, 131.95, 35.03, 112.66, 56.22, 85.61, 56.81)
# GDP of german regions 2015 (in billion EUR)
pop <- pop <- c(10879618, 12843514, 3520031, 2484826, 671489, 1787408, 6176172, 
1612362, 7926599, 17865516, 4052803, 995597, 4084851, 2245470, 2858714, 2170714)
# population of german regions 2015
hoover(gdp, pop)
}
