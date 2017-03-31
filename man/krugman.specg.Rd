\name{krugman.specg}
\alias{krugman.specg}

\title{
Generalized Krugman coefficient of regional specialization
}

\description{
Calculating the Krugman coefficient for the specialization of one region based on regional industry data (normally employment data) compared with a vector of other regions
}

\usage{
krugman.specg(e_ij, e_il)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industries \eqn{i} in region \eqn{j}
}
  \item{e_il}{
a data frame with the employment of the industries \eqn{i} in more that one other region \eqn{l}
}

}
\details{
The \emph{Krugman coefficient of regional specialization} (\eqn{K_{jl}}) is a measure for the dissimilarity of the industrial structure of regions (\eqn{j} and other regions, \eqn{l}) regarding the employment in the \eqn{i} industries in these regions. The coefficient \eqn{K_{jl}} varies between 0 (no specialization/same structure) and 2 (maximum difference, that means there is no single industry localized in both regions).
}

\value{
A single numeric value (\eqn{0 < K_{jl} < 2})
}

\references{
Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Nakamura, R./Morrison Paul, C. J. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 305-328.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{krugman.spec}}
}

\examples{
# Example from Farhauer/Kroell (2013):
Sweden <- c(45000, 15000, 32000, 10000, 30000)
Norway <- c(35000, 12000, 30000, 8000, 22000)
Denmark <- c(40000, 10000, 25000, 9000, 18000)
Finland <- c(30000, 11000, 18000, 3000, 13000)
Island <- c(40000, 6000, 11000, 2000, 12000)
# industry jobs in five industries for five countries
countries <- data.frame(Norway, Denmark, Finland, Island)
# data frame with all comparison countries
krugman.specg(Sweden, countries)
# returns the Krugman coefficient for the specialization
# of sweden compared to Norway, Denmark, Finland and Island
# 0.1595
}