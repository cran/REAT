\name{durpug}
\alias{durpug}

\title{
Relative diversity index by Duranton and Puga
}

\description{
Calculating the relative diversity index (RDI) by Duranton and Puga based on regional industry data (normally employment data)
}

\usage{
durpug(e_ij, e_i)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industries \eqn{i} in region \eqn{j}
}
  \item{e_i}{
a numeric vector with the all-over employment in the industries \eqn{i}
}
}

\value{
A single numeric value of \eqn{RDI}
}

\references{
Duranton, G./Puga, D. (2000): \dQuote{Diversity and Specialisation in Cities: Why, Where and When Does it Matter?}. In: \emph{Urban Studies}, \bold{37}, 3, p. 533-555.

Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini.spec}}, \code{\link{krugman.spec}}, \code{\link{hoover}}
}

\examples{
# Example Goettingen:

data(Goettingen)
# Loads the data

durpug (Goettingen$Goettingen2008[2:13], Goettingen$BRD2008[2:13])
# Returns the Duranton-Puga RDI for Goettingen
}