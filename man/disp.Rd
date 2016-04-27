\name{disp}
\alias{disp}

\title{
Coefficients for disparities
}
\description{
Calculating the Gini coefficient (non-standardized and standardized), the Herfindahl-Hirschman coefficient (non-standardized and standardized) and the Herfindahl-Hirschman equivalent number
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
The \emph{Gini coefficient} and the \emph{Herfindahl-Hirschman coefficient} are measures of the degree of a concentration (e.g. household income, sales or market shares of firms in an industry, distribution of facilities in regions). This function returns both coefficients as non-standardized (\eqn{GINI}, \eqn{HHI}) and standardized values (\eqn{GINI*}, \eqn{HHI*}) and the HHI equivalent number (\eqn{HHI_{eq}}). For more information about the coefficients, see the single function documentations (\emph{gini}, \emph{herf}, \emph{herf.eq}).
}
\value{
a \code{numeric vector} with the five result values (\eqn{HHI, HHI*, HHI_{eq}, GINI, GINI*})
}
\references{
Doersam, P. (2004): \dQuote{Wirtschaftsstatistik anschaulich dargestellt}. Heidenau : PD-Verlag.

Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.cesifo-group.de/link/ifodb_2005_3_25-33.pdf}.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{herf}}, \code{\link{herf.eq}}
}
\examples{
# Example from Doersam (2004)
# (Sales of four car manufacturing firms)
sales <- c(20,50,20,10)
disp(sales)
}