\name{hansen}
\alias{hansen}

\title{
Hansen accessibility
}
\description{
Calculating the Hansen accessibility for given origins and destinations
}
\usage{
hansen(od_dataset, origins, destinations, attrac, dist, gamma = 1, lambda = -2, 
atype = "pow", dtype = "pow", gamma2 = NULL, lambda2 = NULL)
}

\arguments{
  \item{od_dataset}{
an interaction matrix which is a \code{data.frame} containing the origins, destinations, the distances between them and a size variable for the opportunities of the destinations
}
  \item{origins}{
the column in the interaction matrix \code{od_dataset} containing the origins
}
  \item{destinations}{
the column in the interaction matrix \code{od_dataset} containing the destinations
}
  \item{attrac}{
the column in the interaction matrix \code{od_dataset} containing the "attractivity" variable of the destinations (e.g. no. of opportunities)
}
  \item{dist}{
the column in the interaction matrix \code{od_dataset} containing the transport costs (e.g. travelling time)
}
  \item{gamma}{
a single numeric value for the exponential weighting of size (default: 1)
}
  \item{lambda}{
a single numeric value for the exponential weighting of distance (transport costs, default: -2)
}
  \item{atype}{
Type of attractivity weighting function: \code{atype = "pow"} (power function), \code{atype = "exp"} (exponential function) or \code{atype = "logistic"} (default: \code{atype = "pow"})
}
  \item{dtype}{
Type of distance weighting function: \code{dtype = "pow"} (power function), \code{dtype = "exp"} (exponential function) or \code{dtype = "logistic"} (default: \code{dtype = "pow"})
}
  \item{gamma2}{
if \code{atype = "logistic"} a second \eqn{\gamma} parameter is needed
}
  \item{lambda2}{
if \code{dtype = "logistic"} a second \eqn{\lambda} parameter is needed
}
}
\details{
The \emph{Hansen accessibility} (Hansen 1959) can be regarded as a \emph{potential model} of \emph{spatial interaction}. From a theoretical perspective, the accessibility (\eqn{A}) can be seen as the sum of all utilities of every opportunity outgoing from given starting points (\eqn{O_{j}}) and weighted by the distance to them (\eqn{d_{ij}}) (Orpana/Lampinen 2003). Originally the weighting function of distance is not explicitly given and the "attractivities" (e.g. size of the activity at the destinations) is not weighted(\eqn{A_{ij} = \sum_{j}{O_{j} f(d_{ij})}}). This specification is relaxed is this case, so both variables can be weighted by a power (e.g. \eqn{d_{ij}^-\lambda}), exponential or logistic function. 
}
\value{
Returns a \code{data frame} with the origins and the accessibility values (column \code{accessibility}).
}
\references{
Hansen, W. G. (1959): \dQuote{How Accessibility Shapes Land Use}. In: \emph{Journal of the American Institute of Planners}, \bold{25}, 2, p. 73-76. 

Orpana, T./Lampinen, J. (2003): \dQuote{Building spatial choice models from aggregate data}. In: \emph{Journal of Regional Science}, \bold{43}, 2, p. 319-347.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{huff}}
}
\examples{
data(Freiburg1)
data(Freiburg2)
# Loads the data

hansen (Freiburg1, "district", "store", "salesarea", "distance", gamma = 1, lambda = -2)
# Computes the Hansen accessibility from the statistical districts of Freiburg
# regarding the grocery stores and their sizes
}