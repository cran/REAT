\name{krugman.spec}
\alias{krugman.spec}

\title{
Krugman coefficient of regional specialization
}
\description{
Calculating the Krugman coefficient for the specialization of two regions based on regional industry data (normally employment data)
}
\usage{
krugman.spec(e_ij, e_il, e_j, e_l)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industries \eqn{i} in region \eqn{j}
}
  \item{e_il}{
a numeric vector with the employment of the industries \eqn{i} in region \eqn{l}
}
  \item{e_j}{
a single numeric value reflecting the employment in region \eqn{j}
}
  \item{e_l}{
a single numeric value reflecting the employment in region \eqn{l}
}
}
\details{
The \emph{Krugman coefficient of regional specialization} (\eqn{K_{jl}}) is a measure for the dissimilarity of the industrial structure of two regions (\eqn{j} and \eqn{l}) regarding the employment in the \eqn{i} industries in there regions. The coefficient \eqn{K_{jl}} varies between 0 (no specialization/same structure) and 2 (maximum difference, that means there is no single industy localized in both regions).
}
\value{
A single numeric value (\eqn{0 < K_{jl} < 2})
}
\references{
Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

}
\author{
Thomas Wieland
}

\seealso{
\code{\link{gini.conc}}, \code{\link{gini.spec}}
}
\examples{
# Example from Farhauer/Kroell (2013), modified:
E_ij <- c(20,10,70,0,0)
# employment of five industries in region j
E_il <- c(0,0,0,60,40)
# employment of five industries in region l
E_j <- 100
E_l <- 100
# over-all employment in regions j and l
krugman.spec(E_ij, E_il, E_j, E_l)
# results the specialization coefficient (2)
}