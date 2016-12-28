\name{herf}
\alias{herf}

\title{
Herfindahl-Hirschman coefficient
}
\description{
Calculating the Herfindahl-Hirschman coefficient of concentration, standardized and non-standardized
}

\usage{
herf(x, coefnorm = FALSE)
}

\arguments{
  \item{x}{
A numeric vector (e.g. dataset of sales turnover or size of firms)
}
  \item{coefnorm}{
logical argument that indicates if the function output is the non-standardized or the standardized Herfindahl-Hirschman coefficient (default: \code{coefnorm = FALSE}, that means the non-standardized Herfindahl-Hirschman coefficient is returned)
}
}
\details{
The \emph{Herfindahl-Hirschman coefficient} is a popular measure of statistical dispersion, especially used for analyzing concentration in markets, regarding sales turnovers or sizes of \eqn{n} competing firms in an industry. This indicator is e.g. used as a measure of market power and distortions of competition in the governmental competition policy. The coefficient (\eqn{HHI}) varies between \eqn{\frac{1}{n}} (parity resp. no concentration) and \eqn{1} (complete concentration). Because the minimum of \eqn{HHI} is not equal to 0, also a standardized coefficient (\eqn{HHI*}) with a minimum equal to 0 can be calculated alternatively.
}
\value{
A single numeric value of the \emph{Herfindahl-Hirschman coefficient} (\eqn{\frac{1}{n} < HHI < 1}) or the \emph{standardized Herfindahl-Hirschman coefficient} (\eqn{0 < HHI* < 1}).
}

\references{
Doersam, P. (2004): \dQuote{Wirtschaftsstatistik anschaulich dargestellt}. Heidenau : PD-Verlag.

Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.cesifo-group.de/link/ifodb_2005_3_25-33.pdf}.

}
\author{
Thomas Wieland
}

\seealso{
\code{\link{cv}}, \code{\link{gini}}, \code{\link{herf.eq}}
}
\examples{
# Example from Doersam (2004):
sales <- c(20,50,20,10)
# sales turnover of four car manufacturing companies
herf(sales)
# returns the non-standardized HHI (0.34)
herf(sales, coefnorm=TRUE)
# returns the standardized HHI (0.12)
}