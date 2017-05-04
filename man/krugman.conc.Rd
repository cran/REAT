\name{krugman.conc}
\alias{krugman.conc}

\title{
Krugman coefficient of spatial industry concentration for two industries
}

\description{
Calculating the Krugman coefficient for the spatial concentration of two industries based on regional industry data (normally employment data)
}

\usage{
krugman.conc(e_ij, e_uj)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industry \eqn{i} in regions \eqn{j}
}
  \item{e_uj}{
a numeric vector with the employment of the industry \eqn{u} in region \eqn{j}
}

}
\details{
The \emph{Krugman coefficient of industry concentration} (\eqn{K_{iu}}) is a measure for the dissimilarity of the spatial structure of two industries (\eqn{i} and \eqn{u}) regarding the employment in the \eqn{j} regions. The coefficient \eqn{K_{iu}} varies between 0 (no concentration/same structure) and 2 (maximum difference, that means a complete other spatial structure of the industry compared to the others). The calculation is based on the formulae in Farhauer/Kroell (2013).
}

\value{
A single numeric value (\eqn{0 < K_{iu} < 2})
}

\references{
Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Nakamura, R./Morrison Paul, C. J. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 305-328.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{krugman.conc2}}, \code{\link{krugman.spec}}, \code{\link{krugman.spec2}}, \code{\link{locq}} 
}

\examples{
E_ij <- c(4388, 37489, 129423, 60941)
E_uj <- E_ij/2
krugman.conc(E_ij, E_uj)
# exactly the same structure (= no concentration)
}