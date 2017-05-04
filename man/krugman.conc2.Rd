\name{krugman.conc2}
\alias{krugman.conc2}

\title{
Krugman coefficient of spatial industry concentration for more than two industries
}

\description{
Calculating the Krugman coefficient for the spatial concentration of an industry based on regional industry data (normally employment data) compared with a vector of other industries
}

\usage{
krugman.conc2(e_ij, e_uj)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industry \eqn{i} in regions \eqn{j}
}
  \item{e_uj}{
a data frame with the employment of the industry \eqn{u} in \eqn{j} regions
}

}
\details{
The \emph{Krugman coefficient of industry concentration} (\eqn{K_{i}}) is a measure for the dissimilarity of the spatial structure of one industry (\eqn{i}) compared to several others (\eqn{u}) regarding the employment in the \eqn{j} regions. The coefficient \eqn{K_{iu}} varies between 0 (no concentration/same structure) and 2 (maximum difference, that means a complete other spatial structure of the industry compared to the others). The calculation is based on the formulae in Farhauer/Kroell (2013).
}

\value{
A single numeric value (\eqn{0 < K_{i} < 2})
}

\references{
Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Nakamura, R./Morrison Paul, C. J. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 305-328.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{krugman.conc}}, \code{\link{krugman.spec}}, \code{\link{krugman.spec2}}, \code{\link{locq}} 
}

\examples{
# Example from Farhauer/Kroell (2013):
Chemie <- c(20000,11000,31000,8000,20000)
Sozialwesen <- c(40000,10000,25000,9000,16000)
Elektronik <- c(10000,11000,14000,14000,13000)
Holz <- c(7000,7500,11000,1500,36000)
Bergbau <- c(4320, 7811, 3900, 2300, 47560)
# five industries
industries <- data.frame(Chemie, Sozialwesen, Elektronik, Holz)
# data frame with all comparison industries
krugman.conc2(Bergbau, industries)
# returns the Krugman coefficient for the concentration
# of the mining industry (Bergbau) compared to 
# chemistry (Chemie), social services (Sozialwesen), 
# electronics (Elektronik) and wood industry (Holz)
# 0.8619
}