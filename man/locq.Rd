\name{locq}
\alias{locq}

\title{
Location quotient
}
\description{
Calculation the \emph{location quotient}
}
\usage{
locq(e_ij, e_j, e_i, e)
}

\arguments{
  \item{e_ij}{
single numeric value with the employment of industry \eqn{i} in region \eqn{j}
}
  \item{e_j}{
single numeric value with the over-all employment in region \eqn{j}
}
  \item{e_i}{
single numeric value with the over-all employment in industry \eqn{i}
}
  \item{e}{
single numeric value with the over-all employment in all regions
}
}
\details{
The \emph{location quotient} is a simple measure for the concentration of an industry (\eqn{i}) in a region (\eqn{j}) and is also the mathematical basis for other related indicators in regional economics (e.g. \code{gini.conc()}). The function returns the value \eqn{LQ} which is equal to 1 if the concentration of the regarded industry is exactly the same as the over-all concentration (that means, it is proportionally represented in region \eqn{j}). If the value of \eqn{LQ} is smaller (bigger) than 1, the industry is underrepresented (overrepresented). The function checks the input values for errors (i.e. if employment in a region is bigger than over-all employment).
}
\value{
A single numeric value (\eqn{LQ})
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
# Example from Farhauer/Kroell (2013):
locq (1714, 79006, 879213, 15593224)
# returns the location quotient (0.3847623)
}