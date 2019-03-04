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
atype = "pow", dtype = "pow", gamma2 = NULL, lambda2 = NULL, dist_const = 0, 
dist_max = NULL, extract_local = FALSE, accnorm = FALSE, check_df = TRUE,
print.results = TRUE)
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
the column in the interaction matrix \code{od_dataset} containing the transport costs (e.g. travelling time, distance)
}
  \item{gamma}{
a single numeric value for the exponential weighting (\eqn{\gamma}) of size (default: 1)
}
  \item{lambda}{
a single numeric value for the exponential weighting (\eqn{\lambda}) of distance (transport costs, default: -2)
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
  \item{dist_const}{
a \code{numeric} value of a constant to be added to the transport costs (e.g. 1)
}
  \item{dist_max}{
a \code{numeric} value of a maximal value of transport costs for the opportunities to be recognized
}
  \item{extract_local}{
logical argument that indicates if the start points should be included in the analysis or not (if \eqn{i=j}). Default value: \code{extract_local = FALSE}
}
  \item{accnorm}{
logical argument that indicates if the Hansen accessibility should be standardized
}
  \item{check_df}{
logical argument that indicates if the given dataset is checked for correct input, only for internal use, should not be deselected (default: \code{TRUE})
}
  \item{print.results}{
logical argument that indicates if the results are shown (default: \code{TRUE})
}
}

\details{
Accessibility and the inhibiting effect of transport costs on spatial interactions belong to the key concepts of economic geography (Aoyama et al. 2011). The \emph{Hansen accessibility} (Hansen 1959) can be regarded as a \emph{potential model} of \emph{spatial interaction} that describes accessibility as the sum of all opportunities \eqn{O} in the regions \eqn{j}, \eqn{O_{j}}, weighted by distance or other types of transport costs from the origins, \eqn{i}, to them, \eqn{d_{ij}}: \eqn{A_{i} = \sum_{j}{O_{j} f(d_{ij})}}. The distance/travel time is weighted by a distance decay function (\eqn{f(d_{ij})}) to reflect the disutility (opportunity costs) of distance. From a microeconomic perspective, the accessibility of a region or zone can be seen as the sum of all utilities of every opportunity outgoing from given starting points, given an utility function containing the opportunities (utility) and transport costs (disutility) (Orpana/Lampinen 2003). As the accessibility model originally comes from urban land use theory, it can also be used to model spatial concentration/agglomeration, e.g. to quantify the rate of agglomeration of retail locations (Orpana/Lampinen 2003, Wieland 2015). 

Originally the weighting function of distance is not explicitly stated and the "attractivities" (e.g. size of the activity at the destinations) is not weighted. These specifications are relaxed is this function, so both variables can be weighted by a power, exponential or logistic function. If \code{accnorm = TRUE}, the \emph{Hansen accessibility} is standardized by weighting the non-standardized values by the sum of all opportunities without regarding transport costs; the standardized \emph{Hansen accessibility} has a range between 0 and 1.
}

\value{
A \code{list} containing the following objects:
\item{origins }{A data frame containing the origins}
\item{accessibility }{A data frame containing the calculatedaccessibility values (optional: standardized accessibilities)}
}

\references{
Aoyama, Y./Murphy, J. T./Hanson, S. (2011): \dQuote{Key Concepts in Economic Geography}. London : SAGE.

Hansen, W. G. (1959): \dQuote{How Accessibility Shapes Land Use}. In: \emph{Journal of the American Institute of Planners}, \bold{25}, 2, p. 73-76. 

Orpana, T./Lampinen, J. (2003): \dQuote{Building spatial choice models from aggregate data}. In: \emph{Journal of Regional Science}, \bold{43}, 2, p. 319-347.

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{converse}}, \code{\link{dist.calc}}, \code{\link{dist.mat}}, \code{\link{dist.buf}}, \code{\link{huff}}, \code{\link{reilly}}
}

\examples{
# Example from Levy/Weitz (2009):
# Data for the existing and the new location
locations <- c("Existing Store", "New Store")
S_j <- c(5000, 10000)
location_data <- data.frame(locations, S_j)
# Data for the two communities (Rock Creek and Oak Hammock)
communities <- c("Rock Creek", "Oak Hammock")
C_i <- c(5000000, 3000000)
community_data <- data.frame(communities, C_i)
# Combining location and submarket data in the interaction matrix
interactionmatrix <- merge (community_data, location_data)
# Adding driving time:
interactionmatrix[1,5] <- 10
interactionmatrix[2,5] <- 5
interactionmatrix[3,5] <- 5
interactionmatrix[4,5] <- 15
colnames(interactionmatrix) <- c("communities", "C_i", "locations", "S_j", "d_ij")
shoppingcenters1 <- interactionmatrix
huff_shares <- huff(shoppingcenters1, "communities", "locations", "S_j", "d_ij")
# Market shares of the new location:
huff_shares$ijmatrix[huff_shares$ijmatrix$locations == "New Store",]
# Hansen accessibility for Oak Hammock and Rock Creek:
hansen (huff_shares$ijmatrix, "communities", "locations", "S_j", "d_ij")
}