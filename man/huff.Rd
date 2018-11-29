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
localmarket_dataset = NULL, origin_id = NULL, localmarket = NULL, 
check_df = TRUE)
}

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
  \item{localmarket_dataset}{
if \code{output = "total"}, a \code{data.frame} is needed which contains data about the origins 
}
  \item{origin_id}{
the ID variable of the origins in \code{localmarket_dataset}
}
  \item{localmarket}{
the customer/purchasing power potential of the origins in \code{localmarket_dataset}
}
  \item{check_df}{
logical argument that indicates if the given dataset is checked for correct input, only for internal use, should not be deselected (default: \code{TRUE})
}
}

\details{
The \emph{Huff Model} (Huff 1962, 1963, 1964) is the most popular \emph{spatial interaction model} for retailing and services and belongs to the family of \emph{probabilistic market area models}. The basic idea of the model is that consumer decisions are not deterministic but probabilistic, so the decision of customers for a shopping location in a competitive environment cannot be predicted exactly. The results of the model are probabilities for these decisions, which can be interpreted as market shares of the regarded locations (\eqn{j}) in the customer origins (\eqn{i}), \eqn{p_{ij}}, which can be regarded as an equilibrium solution with logically consistent market shares (0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). From a theoretical perspective, the model is based on an utility function with two explanatory variables ("attractivity" of the locations, transport costs between origins and locations), which are weighted by an exponent: \eqn{U_{ij}=A_{j}^\gamma d_{ij}^{-\lambda}}. This specification is relaxed is this case, so both variables can be weighted by a power, exponential or logistic function. 

This function computes the market shares from a given interaction matrix and given weighting parameters. The function returns an estimated interaction matrix. If local market information about the origins (e.g. purchasing power, population size etc.) is stated, the location total turnovers are filed in another \code{data.frame}. Note that each attractivity or distance value must be greater than zero.
}

\value{
A \code{list} containing the following objects:
\item{huffmat }{A data frame containing the Huff interaction matrix}
\item{totals }{If total turnovers are estimated: a data frame containing the total values (turnovers) of each location}
}

\references{
Berman, B. R./Evans, J. R. (2012): \dQuote{Retail Management: A Strategic Approach}. 12th edition. Bosten : Pearson.

Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L. (1963): \dQuote{A Probabilistic Analysis of Shopping Center Trade Areas}. In: \emph{Land Economics}, \bold{39}, 1, p. 81-90.

Huff, D. L. (1964): \dQuote{Defining and Estimating a Trading Area}. In: \emph{Journal of Marketing}, \bold{28}, 4, p. 34-38.

Levy, M./Weitz, B. A. (2012): \dQuote{Retailing management}. 8th edition. New York : McGraw-Hill Irwin.

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272.

Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{converse}}, \code{\link{reilly}}, \code{\link{hansen}}
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
interactionmatrix <- merge (communities, location_data)
# Adding driving time:
interactionmatrix[1,4] <- 10
interactionmatrix[2,4] <- 5
interactionmatrix[3,4] <- 5
interactionmatrix[4,4] <- 15
colnames(interactionmatrix) <- c("communities", "locations", "S_j", "d_ij")

huff_shares <- huff(interactionmatrix, "communities", "locations", "S_j", "d_ij")
huff_shares
# Market shares of the new location:
huff_shares$ijmatrix[huff_shares$ijmatrix$locations == "New Store",]


huff_all <- huff(interactionmatrix, "communities", "locations", "S_j", "d_ij",
localmarket_dataset = community_data, origin_id = "communities", localmarket = "C_i")

huff_all

huff_all$totals
}

\note{
This function contains code from the authors' package MCI.
}