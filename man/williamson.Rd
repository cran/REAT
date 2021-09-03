\name{williamson}
\alias{williamson}

\title{
Williamson index
}

\description{
Calculating the Williamson index (population-weighted coefficient of variation)
}
 
\usage{
williamson (x, weighting, coefnorm = FALSE, wmean = FALSE, na.rm = TRUE)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}
  \item{weighting}{
mandatory: a \code{numeric} vector containing weighting data (usually regional population)
}
  \item{coefnorm}{
logical argument that indicates if the function output is the standardized cv (\eqn{0 < v* < 1}) or not (\eqn{0 < v < \infty}) (default: \code{coefnorm = FALSE})
}
  \item{wmean}{
logical argument that indicates if the weighted mean is used when calculating the weighted coefficient of variation
}
  \item{na.rm}{
logical argument that whether NA values should be extracted or not
}

}
\details{
The \emph{Williamson index} (Williamson 1965) is a population-weighted coefficient of variation.

The \emph{coefficient of variation}, \eqn{v}, is a dimensionless measure of statistical dispersion (\eqn{0 < v < \infty}), based on variance and standard deviation, respectively. The cv (variance, standard deviation) can be weighted by using a second weighting vector. As there is more than one way to weight measures of statistical dispersion, this function uses the formula for the weighted cv (\eqn{v_w}) from Sheret (1984). The cv can be standardized, while this function uses the formula for the standardized cv (\eqn{v*}, with \eqn{0 < v* < 1}) from Kohn/Oeztuerk (2013). The vector \code{x} is automatically treated as a sample (such as in the base \code{sd} function), so the denominator of variance is \eqn{n-1}, if it is not, set \code{is.sample = FALSE}.
}

\value{
Single numeric value. If \code{coefnorm = FALSE} the function returns the non-standardized cv (\eqn{0 < v < \infty}). If \code{coefnorm = TRUE} the standardized cv (\eqn{0 < v* < 1}) is returned. 
}

\references{
Gluschenko, K. (2018): \dQuote{Measuring regional inequality: to weight or not to weight?} In: \emph{Spatial
Economic Analysis}, \bold{13}, 1, p. 36-59.

Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.ifo.de/DocDL/ifodb_2005_3_25-33.pdf}.

Huang, Y./Leung, Y. (2009): \dQuote{Measuring Regional Inequality: A Comparison of Coefficient of Variation and Hoover Concentration Index}. In: \emph{The Open Geography Journal}, \bold{2}, p. 25-34.

Kohn, W./Oeztuerk, R. (2013): \dQuote{Statistik fuer Oekonomen. Datenanalyse mit R und SPSS}. Berlin: Springer.

Portnov, B.A./Felsenstein, D. (2010): \dQuote{On the suitability of income inequality measures for regional analysis: Some evidence from simulation analysis and bootstrapping tests}. In: \emph{Socio-Economic Planning Sciences}, \bold{44}, 4, p. 212-219.

Sheret, M. (1984): \dQuote{The Coefficient of Variation: Weighting Considerations}. In: \emph{Social Indicators Research}, \bold{15}, 3, p. 289-295.

Williamson, J. G. (1965): \dQuote{Regional Inequality and the Process of National Development: A Description of the Patterns}. In: \emph{Economic Development and Cultural Change}, \bold{13}, 4/2, p. 1-84.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{herf}}, \code{\link{hoover}}, \code{\link{cv}}, \code{\link{disp}}
}

\examples{
data(GoettingenHealth2)
# districts with healthcare providers and population size

williamson((GoettingenHealth2$phys_gen/GoettingenHealth2$pop), 
GoettingenHealth2$pop)
}