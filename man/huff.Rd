\name{huff}
\alias{huff}

\title{
Huff model
}
\description{
Calculating market areas using the probabilistic market area model by Huff
}
\usage{
huff(huffdataset, origins, locations, attrac, dist, gamma = 1, lambda = -2, 
atype = "pow", dtype = "pow", gamma2 = NULL, lambda2 = NULL, 
output_total = FALSE, origindataset, od_localmarket)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{huffdataset}{
an interaction matrix which is a \code{data.frame} containing the origins, locations and the explanatory variables
}
  \item{origins}{
the column in the interaction matrix \code{huffdataset} containing the origins (e.g. ZIP codes)
}
  \item{locations}{
the column in the interaction matrix \code{huffdataset} containing the locations (e.g. store codes)
}
  \item{attrac}{
the column in the interaction matrix \code{huffdataset} containing the attractivity variable (e.g. sales area)
}
  \item{dist}{
the column in the interaction matrix \code{huffdataset} containing the transport costs (e.g. travelling time)
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
  \item{output_total}{
logical argument that indicates if the total sales and total shares of the \eqn{j} locations is the output of the function (default: \code{output_total = FALSE})
}
  \item{origindataset}{
a \code{data.frame} containing the origins and additional data about their local market potential (e.g. purchasing power, population, households, ...)
}
  \item{od_localmarket}{
a column in \code{origindataset} containing the data about the local market potential (e.g. purchasing power, population, households, ...)
}
}
\details{
The \emph{Huff model} (Huff 1962, 1963, 1964) is the most popular \emph{spatial interaction model} for retailing and services and belongs to the family of \emph{probabilistic market area models}. The basic idea of the model is that consumer decisions are not deterministic but probabilistic, so the decision of customers for a shopping location in a competitive environment cannot be predicted exactly. The results of the model are probabilities for these decisions, which can be interpreted as market shares of the regarded locations (\eqn{j}) in the customer origins (\eqn{i}), \eqn{p_{ij}}, which can be regarded as an equilibrium solution with logically consistent market shares (0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). From a theoretical perspective, the model is based on an utility function with two explanatory variables ("attractivity" of the locations, transport costs between origins and locations), which are weighted by an exponent: \eqn{U_{ij}=A_{j}^\gamma d_{ij}^-\lambda}. But the weighting functions can also be exponential or logistic. This function computes the market shares from a given interaction matrix and given weighting parameters. If \code{output_total = TRUE} you need local market information about the origins (e.g. purchasing power, population size etc.) filed in another \code{data.frame}, so the function results are the total sales/shares of the given stores/locations. 
}
\value{
Returns either the input interaction matrix including the calculated shares (\code{p_ij}) (if \code{output_total = FALSE}) or the total sales (\code{sum_E_j}) and total shares (\code{share_j}) of the stores locations (if \code{output_total = TRUE}). Both results are \code{data.frame}.
}
\references{
Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L. (1963): \dQuote{A Probabilistic Analysis of Shopping Center Trade Areas}. In: \emph{Land Economics}, \bold{39}, 1, p. 81-90.

Huff, D. L. (1964): \dQuote{Defining and Estimating a Trading Area}. In: \emph{Journal of Marketing}, \bold{28}, 4, p. 34-38.

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272.

Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{converse}}, \code{\link{reilly}}, \code{\link{reilly.lambda}}
}
\examples{
data(Freiburg1)
data(Freiburg2)
# Loads the data

huff (Freiburg1, "district", "store", "salesarea", "distance")
# Standard weighting (power function with gamma=1 and lambda=-2)

huff (Freiburg1, "district", "store", "salesarea", "distance", gamma = 1, lambda = -2, 
atype="pow", dtype="pow", gamma2 = NULL, lambda2 = NULL, output_total = TRUE, Freiburg2, "ppower")
# Calculating total sales of the stores

}