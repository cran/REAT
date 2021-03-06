\name{conc}
\alias{conc}

\title{
Measures of industry concentration
}

\description{
Calculating three measures of industry concentration (Gini, Krugman, Hoover) for a set of \eqn{I} industries
}

\usage{
conc(e_ij, industry.id, region.id, na.rm = TRUE)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industry \eqn{i} in region \eqn{j}
}
  \item{industry.id}{
a vector containing the IDs of the industries \eqn{i}
}
  \item{region.id}{
a vector containing the IDs of the regions \eqn{j}
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
This function is a convenient wrapper for all functions calculating measures of spatial concentration of industries (Gini, Krugman, Hoover)
}

\value{
A \code{matrix} with three columns (Gini coefficient, Krugman coefficient, Hoover coefficient) and \eqn{I} rows (one for each regarded industry).
}

\references{
Farhauer, O./Kroell, A. (2014): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Schaetzl, L. (2000): \dQuote{Wirtschaftsgeographie 2: Empirie}. Paderborn : Schoeningh.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini.conc}}, \code{\link{krugman.conc2}}, \code{\link{hoover}}
}

\examples{
data(G.regions.industries)

conc_i <- conc (e_ij = G.regions.industries$emp_all, 
industry.id = G.regions.industries$ind_code,
region.id = G.regions.industries$region_code)
}