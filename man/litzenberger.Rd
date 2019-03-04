\name{litzenberger}
\alias{litzenberger}

\title{
Litzenberger-Sternberg Cluster Index
}

\description{
Calculating the \emph{Cluster Index} by Litzenberger and Sternberg
}

\usage{
litzenberger(e_ij, e_i, a_j, a, p_j, p, b_ij, b_i)
}

\arguments{
  \item{e_ij}{
a single numeric value with the employment of industry \eqn{i} in region \eqn{j}
}
  \item{e_i}{
a single numeric value with the over-all employment in industry \eqn{i}
}
  \item{a_j}{
a single numeric value of the area of region j 
}
  \item{a}{
a single numeric value of the total area
}
  \item{p_j}{
a single numeric value of the population of region j 
}
  \item{p}{
a single numeric value of the total population 
}
  \item{b_ij}{
a single numeric value of the number of firms of industry \eqn{i} in region \eqn{j}
}
  \item{b_i}{
a single numeric value of the total number of firms of industry \eqn{i}
}
}

\details{
The Litzenberger-Sternberg Cluster Index is not standardized and depends on the number of regarded industries and regions.
}

\value{
A single numeric value of (\eqn{CI}).
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
\code{\link{litzenberger2}}, \code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{locq}}, \code{\link{locq2}}, \code{\link{ellison.a}}, \code{\link{ellison.a2}}, \code{\link{ellison.c}}, \code{\link{ellison.c2}}
}

\examples{
# Example from Farhauer/Kroell (2014):
litzenberger(e_ij = 1743, e_i = 5740, a_j = 50, 
a = 576, p_j = 488, p = 4621, b_ij = 35, b_i = 53)
# 21.87491
}