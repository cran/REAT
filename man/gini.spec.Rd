\name{gini.spec}
\alias{gini.spec}

\title{
Gini coefficient of regional specialization
}
\description{
Calculating the Gini coefficient of regional specialization based on regional industry data (normally employment data)
}
\usage{
gini.spec(e_ij, e_i)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industries \eqn{i} in region \eqn{j}
}
  \item{e_i}{
a numeric vector with the employment in the industries \eqn{i}
}
}
\details{
The \emph{Gini coefficient of regional specialization} (\eqn{G_{j}}) is a special spatial modification of the \emph{Gini coefficient} of inequality (see the function \code{gini()}). It represents the degree of regional specialization of the region \eqn{j} referring to \eqn{i} industries.  The coefficient \eqn{G_{j}} varies between 0 (no specialization) and 1 (complete specialization).
}
\value{
A single numeric value (\eqn{0 < G_{j} < 1})
}
\references{
Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Nakamura, R./Morrison Paul, C. J. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 305-328.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{gini.conc}}
}

\examples{
# Example from Farhauer/Kroell (2013):
E_ij <- c(700,600,500,10000,40000)
# employment of five industries in the region
E_i <- c(30000,15000,10000,60000,50000)
# over-all employment in the five industries
gini.spec (E_ij, E_i)
# Returns the Gini coefficient of regional specialization (0.6222222)

# Example Freiburg
data(Freiburg)
# Loads the data
E_ij <- Freiburg$e_Freiburg2014
# industry-specific employment in Freiburg 2014
E_i <- Freiburg$e_Germany2014
# industry-specific employment in Germany 2014
gini.spec (E_ij, E_i)
# Returns the Gini coefficient of regional specialization (0.2089009)
}
