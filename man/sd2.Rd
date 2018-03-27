\name{sd2}
\alias{sd2}

\title{
Standard deviation (extended)
}

\description{
Calculating the standard deviation (sd), weighted or non-weighted, for samples or populations
}

\usage{
sd2 (x, is.sample = TRUE, weighting = NULL, wmean = FALSE, na.rm = FALSE)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}
  \item{is.sample}{
logical argument that indicates if the dataset is a sample or the population (default: \code{is.sample = TRUE}, so the denominator of variance is \eqn{n-1}) 
}
  \item{weighting}{
a \code{numeric} vector containing weighting data to compute the weighted standard deviation (instead of the non-weighted sd)
}
  \item{wmean}{
logical argument that indicates if the weighted mean is used when calculating the weighted standard deviation
}
  \item{na.rm}{
logical argument that whether NA values should be extracted or not
}
}

\details{
The function calculates the \emph{standard deviation}. Unlike the R base \code{sd} function, the \code{sd2} function allows to choose if the data is treated as sample (denominator of variance is \eqn{n-1})) or not (denominator of variance is \eqn{n}))

From a regional economic perspective, the sd is closely linked to the concept of \emph{sigma convergence} (\eqn{\sigma}) which means a harmonization of regional economic output or income over time, while the other type of convergence, \emph{beta convergence} (\eqn{\beta}), means a decline of dispersion because poor regions have a stronger growth than rich regions (Capello/Nijkamp 2009). The sd allows to summarize regional disparities (e.g. disparities in regional GDP per capita) in one indicator. The coefficient of variation (see the function \code{cv}) is more frequently used for this purpose (e.g. Lessmann 2005, Huang/Leung 2009, Siljak 2015). But the sd can also be used for any other types of disparities or dispersion, such as disparities in supply (e.g. density of physicians or grocery stores). 

The standard deviation can be weighted by using a second weighting vector. As there is more than one way to weight measures of statistical dispersion, this function uses the formula for the weighted sd (\eqn{\sigma_w}) from Sheret (1984). The vector \code{x} is automatically treated as a sample (such as in the base \code{sd} function), so the denominator of variance is \eqn{n-1}, if it is not, set \code{is.sample = FALSE}.  
}

\value{
Single numeric value. If \code{weighting} is specified, the function returns a weighted standard deviation (optionally using a weighted arithmetic mean if \code{wmean = TRUE}). 
}

\references{
Bahrenberg, G./Giese, E./Mevenkamp, N./Nipper, J. (2010): \dQuote{Statistische Methoden in der Geographie. Band 1: Univariate und bivariate Statistik}. Stuttgart: Borntraeger.

Capello, R./Nijkamp, P. (2009): \dQuote{Introduction: regional growth and development theories in the twenty-first century - recent theoretical advances and future challenges}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 1-16.

Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.cesifo-group.de/DocDL/ifodb_2005_3_25-33.pdf}.

Huang, Y./Leung, Y. (2009): \dQuote{Measuring Regional Inequality: A Comparison of Coefficient of Variation and Hoover Concentration Index}. In: \emph{The Open Geography Journal}, \bold{2}, p. 25-34.

Sheret, M. (1984): \dQuote{The Coefficient of Variation: Weighting Considerations}. In: \emph{Social Indicators Research}, \bold{15}, 3, p. 289-295.

Siljak, D. (2015): \dQuote{Real Economic Convergence in Western Europe from 1995 to 2013}. In: \emph{International Journal of Business and Economic Development}, \bold{3}, 3, p. 56-67.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{herf}}, \code{\link{hoover}}, \code{\link{mean2}}, \code{\link{rca}}
}

\examples{
# Regional disparities / sigma convergence in Germany
data(G.counties.gdp)
# GDP per capita for German counties (Landkreise)
sd_gdppc <- apply (G.counties.gdp[54:68], MARGIN = 2, FUN = sd2)
# Calculating standard deviation for the years 2000-2014
years <- 2000:2014
# vector of years (2000-2014)
plot(years, sd_gdppc, "l", ylim = c(0,15000), xlab = "Year", 
ylab = "SD of GDP per capita")
# Plot sd over time
}