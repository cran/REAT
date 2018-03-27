\name{cv}
\alias{cv}

\title{
Coefficient of variation
}
\description{
Calculating the coefficient of variation (cv), standardized and non-standardized, weighted and non-weighted
}
\usage{
cv (x, is.sample = TRUE, coefnorm = FALSE, weighting = NULL, 
wmean = FALSE, na.rm = FALSE)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}
  \item{is.sample}{
logical argument that indicates if the dataset is a sample or the population (default: \code{is.sample = TRUE}, so the denominator of variance is \eqn{n-1}) 
}
  \item{coefnorm}{
logical argument that indicates if the function output is the standardized cv (\eqn{0 < v* < 1}) or not (\eqn{0 < v < \infty}) (default: \code{coefnorm = FALSE})
}
  \item{weighting}{
a \code{numeric} vector containing weighting data to compute the weighted coefficient of variation (instead of the non-weighted cv)
}
  \item{wmean}{
logical argument that indicates if the weighted mean is used when calculating the weighted coefficient of variation
}
  \item{na.rm}{
logical argument that whether NA values should be extracted or not
}

}
\details{
The \emph{coefficient of variation}, \eqn{v}, is a dimensionless measure of statistical dispersion (\eqn{0 < v < \infty}), based on variance and standard deviation, respectively. From a regional economic perspective, it is closely linked to the concept of \emph{sigma convergence} (\eqn{\sigma}) which means a harmonization of regional economic output or income over time, while the other type of convergence, \emph{beta convergence} (\eqn{\beta}), means a decline of dispersion because poor regions have a stronger growth than rich regions (Capello/Nijkamp 2009). The cv allows to summarize regional disparities (e.g. disparities in regional GDP per capita) in one indicator and is more frequently used for this purpose than the standard deviation, especially in analyzing of \eqn{\sigma} convergence over a long period (e.g. Lessmann 2005, Huang/Leung 2009, Siljak 2015). But the cv can also be used for any other types of disparities or dispersion, such as disparities in supply (e.g. density of physicians or grocery stores). 

The cv (variance, standard deviation) can be weighted by using a second weighting vector. As there is more than one way to weight measures of statistical dispersion, this function uses the formula for the weighted cv (\eqn{v_w}) from Sheret (1984). The cv can be standardized, while this function uses the formula for the standardized cv (\eqn{v*}, with \eqn{0 < v* < 1}) from Kohn/Oeztuerk (2013). The vector \code{x} is automatically treated as a sample (such as in the base \code{sd} function), so the denominator of variance is \eqn{n-1}, if it is not, set \code{is.sample = FALSE}.  
}

\value{
Single numeric value. If \code{coefnorm = FALSE} the function returns the non-standardized cv (\eqn{0 < v < \infty}). If \code{coefnorm = TRUE} the standardized cv (\eqn{0 < v* < 1}) is returned. 
}

\references{
Bahrenberg, G./Giese, E./Mevenkamp, N./Nipper, J. (2010): \dQuote{Statistische Methoden in der Geographie. Band 1: Univariate und bivariate Statistik}. Stuttgart: Borntraeger.

Capello, R./Nijkamp, P. (2009): \dQuote{Introduction: regional growth and development theories in the twenty-first century - recent theoretical advances and future challenges}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 1-16.

Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.cesifo-group.de/DocDL/ifodb_2005_3_25-33.pdf}.

Huang, Y./Leung, Y. (2009): \dQuote{Measuring Regional Inequality: A Comparison of Coefficient of Variation and Hoover Concentration Index}. In: \emph{The Open Geography Journal}, \bold{2}, p. 25-34.

Kohn, W./Oeztuerk, R. (2013): \dQuote{Statistik fuer Oekonomen. Datenanalyse mit R und SPSS}. Berlin: Springer.

Sheret, M. (1984): \dQuote{The Coefficient of Variation: Weighting Considerations}. In: \emph{Social Indicators Research}, \bold{15}, 3, p. 289-295.

Siljak, D. (2015): \dQuote{Real Economic Convergence in Western Europe from 1995 to 2013}. In: \emph{International Journal of Business and Economic Development}, \bold{3}, 3, p. 56-67.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{herf}}, \code{\link{hoover}}, \code{\link{rca}}
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
