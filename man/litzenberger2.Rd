\name{litzenberger2}
\alias{litzenberger2}

\title{
Litzenberger-Sternberg Cluster Index
}

\description{
Calculating the \emph{Cluster Index} by Litzenberger and Sternberg for a given number of \eqn{I} industries and \eqn{J} regions
}

\usage{
litzenberger2(e_ij, industry.id, region.id, a_j, p_j, b_ij, 
CI.output = "mat", na.rm = TRUE)
}

\arguments{
  \item{e_ij}{
a vector with the employment of industry \eqn{i} in region \eqn{j}
}
  \item{industry.id}{
a vector containing the IDs of the industries \eqn{i}
}
  \item{region.id}{
a vector containing the IDs of the regions \eqn{j}
}
  \item{a_j}{
a vector containing the areas of the regions \eqn{j}
}
  \item{p_j}{
a vector containing the populations of the regions \eqn{j} 
}
  \item{b_ij}{
a vector containing the numbers of firms of industry \eqn{i} in region \eqn{j}
}
  \item{CI.output}{
Type of output: matrix (default: \code{CI.output = "mat"}) or data frame (\code{CI.output = "df"})
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
The Litzenberger-Sternberg Cluster Index is not standardized and depends on the number of regarded industries and regions.
}

\value{
A matrix or data frame containing \eqn{IxJ} values of \eqn{CI}
}

\references{
Farhauer, O./Kroell, A. (2014): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Hoffmann J./Hirsch, S./Simons, J. (2017): \dQuote{Identification of spatial agglomerations in the German food processing industry}. In: \emph{Papers in Regional Science}, \bold{96}, 1, p. 139-162.

Litzenberger, T./Sternberg, R. (2006): \dQuote{Der Clusterindex - eine Methodik zur Identifizierung regionaler Cluster am Beispiel deutscher Industriebranchen}. In: \emph{Geographische Zeitschrift}, \bold{94}, 2, p. 209-224.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{litzenberger}}, \code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{locq}}, \code{\link{locq2}}, \code{\link{ellison.a}}, \code{\link{ellison.a2}}, \code{\link{ellison.c}}, \code{\link{ellison.c2}}
}

\examples{
data (G.regions.industries)

lss <- litzenberger2(G.regions.industries$emp_all, 
G.regions.industries$ind_code, G.regions.industries$region_code,
G.regions.industries$area_sqkm, G.regions.industries$pop,
G.regions.industries$firms, CI.output = "df")
# output as data frame

lss_sort <- lss[order(lss$CI, decreasing = TRUE),]
# Sort decreasing by size of CI

lss_sort[1:5,]
}