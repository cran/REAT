\name{converse}
\alias{converse}

\title{
Breaking point formula by Converse 
}
\description{
Calculating the breaking point between two cities or retail locations   
}
\usage{
converse(P_a, P_b, D_ab)
}
\arguments{
  \item{P_a}{
a single numeric value of attractivity/population size of location/city \eqn{a}
}
  \item{P_b}{
a single numeric value of attractivity/population size of location/city \eqn{b}
}
  \item{D_ab}{
a single numeric value of the transport costs (e.g. distance) between \eqn{a} and \eqn{b}
}
}
\details{
The \emph{breaking point formula} by Converse (1949) is a modification of the \emph{law of retail gravitation} by Reilly (1929, 1931) (see the functions \code{reilly} and \code{reilly.lambda}). The aim of the calculation is to determine the boundaries of the market areas between two locations/cities in consideration of their attractivity/population size and the transport costs (e.g. distance) between them. The models by Reilly and Converse are simple \emph{spatial interaction models} and are considered as \emph{deterministic market area models} due to their exact allocation of demand origins to locations. A probabilistic approach including a theoretical framework was developed by Huff (1962) (see the function \code{huff}).
}
\value{
a list with two values (\code{B_a}: distance from location \eqn{a} to breaking point, \code{B_b}: distance from location \eqn{b} to breaking point)
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
\code{\link{huff}}, \code{\link{reilly}}, \code{\link{reilly.lambda}}
}
\examples{
# Example from Huff (1962):
converse (400000, 200000, 80)
# two cities (population 400.000 and 200.000 with a distance separating them of 80 miles)
}
