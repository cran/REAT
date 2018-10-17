\name{herf}
\alias{herf}

\title{
Herfindahl-Hirschman coefficient
}
\description{
Calculating the Herfindahl-Hirschman coefficient of concentration, standardized and non-standardized
}

\usage{
herf(x, coefnorm = FALSE, output = "HHI", na.rm = TRUE)
}

\arguments{
  \item{x}{
A numeric vector (e.g. dataset of sales turnover or size of firms)
}
  \item{coefnorm}{
logical argument that indicates if the function output is the non-standardized or the standardized Herfindahl-Hirschman coefficient (default: \code{coefnorm = FALSE}, that means the non-standardized Herfindahl-Hirschman coefficient is returned)
}
  \item{output}{
argument to state the output. If \code{output = "HHI"} (default), the Herfindahl-Hirschman coefficient is returned (standardized or non-standardized). If \code{output = "eq"}, the Herfindahl-Hirschman coefficient equivalent number is returned
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
The \emph{Herfindahl-Hirschman coefficient} is a popular measure of statistical dispersion, especially used for analyzing concentration in markets, regarding sales turnovers or sizes of \eqn{n} competing firms in an industry. This indicator is especially used as a measure of market power and distortions of competition in the governmental competition policy (Roberts 2014). But the coefficient is also utilized as a measure of geographic concentration of industries (Lessmann 2005, Nakamura/Morrison Paul 2009).

The coefficient (\eqn{HHI}) varies between \eqn{\frac{1}{n}} (parity resp. no concentration) and \eqn{1} (complete concentration). Because the minimum of \eqn{HHI} is not equal to 0, also a standardized coefficient (\eqn{HHI*}) with a minimum equal to 0 can be calculated alternatively. The \emph{equivalent number} (which is the inverse of the \emph{Herfindahl-Hirschman coefficient}) reflects the theoretical number of economic objects (normally firms) where a calculated coefficient is \eqn{\frac{1}{n}}, which means parity (Doersam 2004). In a regional context, the inverse of HHI is also used as a measure of diversity (Duranton/Puga 2000).
}

\value{
A single numeric value of the \emph{Herfindahl-Hirschman coefficient} (\eqn{\frac{1}{n} < HHI < 1}) or the \emph{standardized Herfindahl-Hirschman coefficient} (\eqn{0 < HHI* < 1}) or the \emph{Herfindahl-Hirschman coefficient equivalent number} (\eqn{H_{eq} >= 1}).
}

\references{
Doersam, P. (2004): \dQuote{Wirtschaftsstatistik anschaulich dargestellt}. Heidenau : PD-Verlag.

Duranton, G./Puga, D. (2000): \dQuote{Diversity and Specialisation in Cities: Why, Where and When Does it Matter?}. In: \emph{Urban Studies}, \bold{37}, 3, p. 533-555. 

Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.cesifo-group.de/DocDL/ifodb_2005_3_25-33.pdf}.

Nakamura, R./Morrison Paul, C. J. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 305-328.

Roberts, T. (2014): \dQuote{When Bigger Is Better: A Critique of the Herfindahl-Hirschman Index's Use to Evaluate Mergers in Network Industries}. In: \emph{Pace Law Review}, \bold{34}, 2, p. 894-946.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{cv}}, \code{\link{gini}}
}

\examples{
# Example from Doersam (2004):
sales <- c(20,50,20,10)
# sales turnover of four car manufacturing companies
herf(sales)
# returns the non-standardized HHI (0.34)
herf(sales, coefnorm=TRUE)
# returns the standardized HHI (0.12)
herf(sales, output = "eq")
# returns the HHI equivalent number (2.94)

# Regional disparities in Germany:
gdp <- c(460.69, 549.19, 124.16, 65.29, 31.59, 109.27, 263.44, 39.87, 258.53, 
645.59, 131.95, 35.03, 112.66, 56.22, 85.61, 56.81)
# GDP of german regions 2015 (in billion EUR)
herf(gdp)
# returns the HHI (0.125)
}