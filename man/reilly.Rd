\name{reilly}
\alias{reilly}

\title{
Law of retail gravitation by Reilly
}
\description{
Calculating the proportion of sales from an intermediate town between two cities or retail locations
}
\usage{
reilly(P_a, P_b, D_a, D_b, gamma = 1, lambda = 2, relation = NULL)
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
  \item{gamma}{
a single numeric value for the exponential weighting of size (default: 1)
}
  \item{lambda}{
a single numeric value for the exponential weighting of distance (transport costs, default: -2)
}
  \item{relation}{
a single numeric value containing the relation of trade between cities/locations \eqn{a} and \eqn{b} (only needed if the distance decay parameters has to be estimated instead of the sales flows)
}

}
\details{
The \emph{law of retail gravitation} by Reilly (1929, 1931) was the first \emph{spatial interaction model} for retailing and services. This "law" states that two cities/locations attract customers from an intermediate town proportionally to the attractivity/population size of the two cities/locations and in inverse proportion to the squares of the transport costs (e.g. distance, travelling time) from these two locations to the intermediate town. But both variables can be weighted by exponents. The distance exponent can also be derived from empirical data (if an empirical \code{relation} is stated). The \emph{breaking point formula} by Converse (1949) is a separate transformation of Reilly's law (see the function \code{converse}). The models by Reilly and Converse are simple \emph{spatial interaction models} and are considered as \emph{deterministic market area models} due to their exact allocation of demand origins to locations. A probabilistic approach including a theoretical framework was developed by Huff (1962) (see the function \code{huff}).
}

\value{
If no relation is stated, a \code{list} with three values: 
\item{relation_AB}{relation of trade between cities/locations \eqn{a} and \eqn{b}}
\item{prop_A}{proportion of city/location \eqn{a}}
\item{prop_B}{proportion of city/location \eqn{b}}

If a relation is stated instead of weighting parameters, a single numeric value containing the estimated distance decay parameter.
}

\references{
Berman, B. R./Evans, J. R. (2012): \dQuote{Retail Management: A Strategic Approach}. 12th edition. Bosten : Pearson.

Converse, P. D. (1949): \dQuote{New Laws of Retail Gravitation}. In: \emph{Journal of Marketing}, \bold{14}, 3, p. 379-384.

Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Levy, M./Weitz, B. A. (2012): \dQuote{Retailing management}. 8th edition. New York : McGraw-Hill Irwin.

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272

Reilly, W. J. (1929): \dQuote{Methods for the Study of Retail Relationships}. \emph{Studies in Marketing}, \bold{4}. Austin : Bureau of Business Research, The University of Texas.

Reilly, W. J. (1931): \dQuote{The Law of Retail Gravitation}. New York.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{huff}}, \code{\link{converse}}
}

\examples{
# Example from Converse (1949):
reilly (39851, 37366, 27, 25)
# two cities (pop. size 39.851 and 37.366) 
# with distances of 27 and 25 miles to intermediate town
myresults <- reilly (39851, 37366, 27, 25)
myresults$prop_A
# proportion of location a
# Distance decay parameter for the given sales relation:
reilly (39851, 37366, 27, 25, gamma = 1, lambda = NULL, relation = 0.9143555)   
# returns 2
}