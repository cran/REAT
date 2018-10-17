\name{krugman.spec}
\alias{krugman.spec}

\title{
Krugman coefficient of regional specialization for two regions
}
\description{
Calculating the Krugman coefficient for the specialization of two regions based on regional industry data (normally employment data)
}
\usage{
krugman.spec(e_ij, e_il)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industries \eqn{i} in region \eqn{j}
}
  \item{e_il}{
a numeric vector with the employment of the industries \eqn{i} in region \eqn{l}
}

}
\details{
The \emph{Krugman coefficient of regional specialization} (\eqn{K_{jl}}) is a measure for the dissimilarity of the industrial structure of two regions (\eqn{j} and \eqn{l}) regarding the employment in the \eqn{i} industries in these regions. The coefficient \eqn{K_{jl}} varies between 0 (no specialization/same structure) and 2 (maximum difference, that means there is no single industry localized in both regions). The calculation is based on the formulae in Farhauer/Kroell (2013).
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
\code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{krugman.conc}}, \code{\link{krugman.conc2}}, \code{\link{krugman.spec2}}, \code{\link{locq}}
}

\examples{
# Example from Farhauer/Kroell (2013), modified:
E_ij <- c(20,10,70,0,0)
# employment of five industries in region j
E_il <- c(0,0,0,60,40)
# employment of five industries in region l
krugman.spec(E_ij, E_il)
# results the specialization coefficient (2)

# Example Goettingen:
data(Goettingen)
krugman.spec(Goettingen$Goettingen2017[2:16], Goettingen$BRD2017[2:16])
# Returns the Krugman coefficient of regional specialization 2017 (0.4508469)
}