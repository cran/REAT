\name{locq}
\alias{locq}

\title{
Location quotient
}

\description{
Calculating the \emph{location quotient} (a.k.a. \emph{Hoover-Balassa quotient})
}

\usage{
locq(e_ij, e_j, e_i, e, industry.names = NULL, plot.results = FALSE,
LQ.method = "m", plot.title = "Localization quotients", 
bar.col = "lightblue", line.col = "red", arg.size = 1)
}

\arguments{
  \item{e_ij}{
a single numeric value or vector with the employment of industry/industries \eqn{i} in region \eqn{j}
}
  \item{e_j}{
a single numeric value with the over-all employment in region \eqn{j}
}
  \item{e_i}{
a single numeric value or vector with the over-all employment in industry/industries \eqn{i}
}
  \item{e}{
a single numeric value with the over-all employment in all regions
}
  \item{industry.names}{
Industry names (e.g. from the relevant statistical classification of economic activities)
}
  \item{plot.results}{
Logical argument that indicates if the results have to be plotted (only available if \eqn{i > 1})
}
  \item{LQ.method}{
Indicates whether the multiplicative (default: \code{LQ.method = "m"}) or the additive LQ (\code{LQ.method = "m"}) is computed
}
  \item{plot.title}{
If \code{plot.results = TRUE}: Plot title 
}
  \item{bar.col}{
If \code{plot.results = TRUE}: Bar colour 
}
  \item{line.col}{
If \code{plot.results = TRUE}: LQ1-line colour 
}
  \item{arg.size}{
If \code{plot.results = TRUE}: Size of industry names in bar plot 
}
}

\details{
The \emph{location quotient} is a simple measure for the concentration of an industry (\eqn{i}) in a region (\eqn{j}) and is also the mathematical basis for other related indicators in regional economics (e.g. \code{gini.conc()}). The function returns the value \eqn{LQ} which is equal to 1 if the concentration of the regarded industry is exactly the same as the over-all concentration (that means, it is proportionally represented in region \eqn{j}). If the value of \eqn{LQ} is smaller (bigger) than 1, the industry is underrepresented (overrepresented). The function checks the input values for errors (i.e. if employment in a region is bigger than over-all employment).
}

\value{
A single numeric value of (\eqn{LQ}) or a matrix with respect to all \eqn{i} industries. Optional: plot.
}

\references{
Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Hoen A.R./Oosterhaven, J. (2006): \dQuote{On the measure of comparative advantage}. In: \emph{The Annals of Regional Science}, \bold{40}, 3, p. 677-691.

Nakamura, R./Morrison Paul, C. J. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 305-328.

O'Donoghue, D./Gleave, B. (2004): \dQuote{A Note on Methods for Measuring Industrial Agglomeration}. In: \emph{Regional Studies}, \bold{38}, 4, p. 419-427.

Tian, Z. (2013): \dQuote{Measuring agglomeration using the standardized location quotient with a bootstrap method}. In: \emph{Journal of Regional Analysis and Policy}, \bold{43}, 2, p. 186-197.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{locq2}}
}

\examples{
# Example from Farhauer/Kroell (2013):
locq (1714, 79006, 879213, 15593224)
# returns the location quotient (0.3847623)

# Location quotients for Goettingen 2017:
data(Goettingen)
locq (Goettingen$Goettingen2017[2:16], Goettingen$Goettingen2017[1], 
Goettingen$BRD2017[2:16], Goettingen$BRD2017[1])
}