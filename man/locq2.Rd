\name{locq2}
\alias{locq2}

\title{
Location quotient
}

\description{
Calculating the \emph{location quotient} (a.k.a. \emph{Hoover-Balassa quotient}) for a given number of \eqn{I} industries and \eqn{J} regions
}

\usage{
locq2(e_ij, industry.id, region.id, LQ.norm = "none", 
LQ.output = "mat", na.rm = TRUE)
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
  \item{LQ.norm}{
Type of normalization of the location quotients: no normalization (default: \code{LQ.norm = "none"}), z values (\code{LQ.norm = "OG"}) or z values of logged location quotients (\code{LQ.norm = "T"})
}
  \item{LQ.output}{
Type of output: matrix (default: \code{CI.output = "mat"}) or data frame (\code{CI.output = "df"})
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
The \emph{location quotient} is a simple measure for the concentration of an industry (\eqn{i}) in a region (\eqn{j}) and is also the mathematical basis for other related indicators in regional economics (e.g. \code{gini.conc()}). The function returns the value \eqn{LQ} which is equal to 1 if the concentration of the regarded industry is exactly the same as the over-all concentration (that means, it is proportionally represented in region \eqn{j}). If the value of \eqn{LQ} is smaller (bigger) than 1, the industry is underrepresented (overrepresented). The function checks the input values for errors (i.e. if employment in a region is bigger than over-all employment).

Two types of normalization are available: z values of the location quotients (O'Donoghue/Gleave 2004) or z values of logged location quotients (Tian 2013). 
}

\value{
A matrix or data frame containing \eqn{IxJ} values of \eqn{LQ}
}

\references{
Farhauer, O./Kroell, A. (2014): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Hoen A.R./Oosterhaven, J. (2006): \dQuote{On the measure of comparative advantage}. In: \emph{The Annals of Regional Science}, \bold{40}, 3, p. 677-691.

Nakamura, R./Morrison Paul, C. J. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 305-328.

O'Donoghue, D./Gleave, B. (2004): \dQuote{A Note on Methods for Measuring Industrial Agglomeration}. In: \emph{Regional Studies}, \bold{38}, 4, p. 419-427.

Tian, Z. (2013): \dQuote{Measuring agglomeration using the standardized location quotient with a bootstrap method}. In: \emph{Journal of Regional Analysis and Policy}, \bold{43}, 2, p. 186-197.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{litzenberger}}, \code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{locq}}, \code{\link{hoover}}, \code{\link{ellison.a}}, \code{\link{ellison.a2}}, \code{\link{ellison.c}}, \code{\link{ellison.c2}}
}

\examples{
data (G.regions.industries)

lqs <- locq2(e_ij = G.regions.industries$emp_all, 
G.regions.industries$ind_code, G.regions.industries$region_code, 
LQ.output = "df")
# output as data frame

lqs_sort <- lqs[order(lqs$LQ, decreasing = TRUE),]
# Sort decreasing by size of LQ

lqs_sort[1:5,]
}