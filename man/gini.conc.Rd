\name{gini.conc}
\alias{gini.conc}

\title{
Gini coefficient of spatial industry concentration
}
\description{
Calculating the Gini coefficient of spatial industry concentration based on regional industry data (normally employment data)
}
\usage{
gini.conc(e_ij, e_j)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industry \eqn{i} in region \eqn{j}
}
  \item{e_j}{
a numeric vector with the employment in region \eqn{j}
}
}
\details{
The \emph{Gini coefficient of spatial industry concentration} (\eqn{G_{i}}) is a special spatial modification of the Gini coefficient of inequality (see the function \code{gini()}). It represents the rate of spatial concentration of the industry \eqn{i} referring to \eqn{j} regions (e.g. cities, counties, states). The coefficient \eqn{G_{i}} varies between 0 (perfect distribution, respectively no concentration) and 1 (complete concentration in one region).  
}
\value{
A single numeric value (\eqn{0 < G_{i} < 1})
}
\references{
Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.
}
\author{
Thomas Wieland
}
\seealso{
\code{\link{gini}}, \code{\link{gini.spec}}
}

\examples{
# Example from Farhauer/Kroell (2013):
E_ij <- c(500,500,1000,7000,1000)
# employment of the industry in five regions
E_j <- c(20000,15000,20000,40000,5000)
# employment in the five regions
gini.conc (E_ij, E_j)
# Returns the Gini coefficient of industry concentration (0.4068966)

}
