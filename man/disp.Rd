\name{disp}
\alias{disp}

\title{
Coefficients for disparities
}
\description{
Calculating the Gini coefficient (non-standardized and standardized), the Herfindahl-Hirschman coefficient (non-standardized and standardized) and the Herfindahl-Hirschman equivalent number and the coefficient of variation (non-standardized and standardized)
}
\usage{
disp(x)
}

\arguments{
  \item{x}{
a \code{numeric vector} containing the regarded objects
}
}
\details{
The \emph{Gini coefficient} and the \emph{Herfindahl-Hirschman coefficient} are measures of the degree of a concentration (e.g. household income, sales or market shares of firms in an industry, distribution of facilities in regions). The \emph{coefficient of variation} is a simple standardized measure of distribution. This function returns these coefficients as non-standardized (\eqn{G}, \eqn{HHI}, \eqn{CV}) and standardized values (\eqn{G*}, \eqn{HHI*}, \eqn{CV*}) and the HHI equivalent number (\eqn{HHI_{eq}}). For more information about the coefficients, see the single function documentations (\code{gini}, \code{herf}, \code{herf.eq} and \code{cv}).
}
\value{
a \code{list} with the 7 entires (=result values):
\item{HHI}{Herfindahl-Hirschman coefficient, non-standardized}
\item{HHIn}{Herfindahl-Hirschman coefficient, standardized}
\item{HHIeq}{Herfindahl-Hirschman equivalent number}
\item{GINI}{Gini coefficient, non-standardized}
\item{GINIn}{Gini coefficient, standardized}
\item{CV}{Coefficient of variation, non-standardized}
\item{CVn}{Coefficient of variation, standardized}
}

\references{
Bahrenberg, G./Giese, E./Mevenkamp, N./Nipper, J. (2010): \dQuote{Statistische Methoden in der Geographie. Band 1: Univariate und bivariate Statistik}. Stuttgart: Borntraeger.

Doersam, P. (2004): \dQuote{Wirtschaftsstatistik anschaulich dargestellt}. Heidenau : PD-Verlag.

Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.cesifo-group.de/link/ifodb_2005_3_25-33.pdf}.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{herf}}, \code{\link{cv}}
}

\examples{
# Example from Doersam (2004)
# (Sales of four car manufacturing firms)
sales <- c(20,50,20,10)
disp(sales)

# Supply and distribution of physicians:
data(health4)
# Medical supply in the 420 regions
disp(health4$phys_gen)

}