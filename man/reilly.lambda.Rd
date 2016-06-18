\name{reilly.lambda}
\alias{reilly.lambda}

\title{
Distance exponent in law of retail gravitation by Reilly
}
\description{
Deriving the distance exponent in Reilly's law of retail gravitation from empirical data
}
\usage{
reilly.lambda(P_a, P_b, D_a, D_b, B_a_div_B_b)
}

\arguments{
  \item{P_a}{
a single numeric value of attractivity/population size of location/city \eqn{a}
}
  \item{P_b}{
a single numeric value of attractivity/population size of location/city \eqn{b}
}
  \item{D_a}{
a single numeric value of the distance from the intermediate town to location/city \eqn{a}
}
  \item{D_b}{
a single numeric value of the distance from the intermediate town to location/city \eqn{b}
}
  \item{B_a_div_B_b}{
relation of trade between cities/locations \eqn{a} and \eqn{b}
}
}
\details{
The \emph{law of retail gravitation} by Reilly (1929, 1931) was the first \emph{spatial interaction model} for retailing and services. This "law" states that two cities/locations attract customers from an intermediate town proportionally to the attractivity/population size of the two cities/locations and in inverse proportion to the squares of the transport costs (e.g. distance, travelling time) from these two locations to the intermediate town. But both variables can be weighted by exponents (see the function \code{reilly}). The distance exponent (lambda) can also be derived from empirical data. The \emph{breaking point formula} by Converse (1949) is a separate transformation of Reilly's law (see the function \code{converse}). The models by Reilly and Converse are simple \emph{spatial interaction models} and are considered as \emph{deterministic market area models} due to their exact allocation of demand origins to locations. A probabilistic approach including a theoretical framework was developed by Huff (1962) (see the function \code{huff}).
}
\value{
a single numeric value (lambda)
}
\references{
Converse, P. D. (1949): \dQuote{New Laws of Retail Gravitation}. In: \emph{Journal of Marketing}, \bold{14}, 3, p. 379-384.

Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272

Reilly, W. J. (1929): \dQuote{Methods for the Study of Retail Relationships}. \emph{Studies in Marketing}, \bold{4}. Austin : Bureau of Business Research, The University of Texas.

Reilly, W. J. (1931): \dQuote{The Law of Retail Gravitation}. New York.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{reilly}}, \code{\link{huff}}, \code{\link{converse}}
}

\examples{
# Example from Converse (1949):
reilly.lambda (39851, 37366, 27, 25, 0.9143555)
# returns lambda of about 2
}