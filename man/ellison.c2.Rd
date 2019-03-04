\name{ellison.c2}
\alias{ellison.c2}

\title{
Ellison-Glaeser Coagglomeration Index
}

\description{
Calculating the Coagglomeration Index by Ellison and Glaeser for \eqn{IxI} sets of two industries
}

\usage{
ellison.c2(e_ik, industry, region, e_j = NULL, print.results = TRUE)
}

\arguments{
  \item{e_ik}{
a numeric vector containing the no. of employees of firm \eqn{k} from industry \eqn{i}
}
  \item{industry}{
a vector containing the IDs/names of the industries \eqn{i}
}
  \item{region}{
a vector containing the IDs/names of the regions \eqn{j}
}
  \item{e_j}{
a numeric vector containing the total employment of the regions \eqn{j}
}
  \item{print.results}{
logical argument that indicates whether the results are printed or not (for internal use)
}
}

\details{
The Ellison-Glaeser Coagglomeration Index is not standardized. A value of \eqn{\gamma^c = 0} indicates a spatial distribution of firms equal to a dartboard approach. Values below zero indicate spatial dispersion, values greater than zero indicate clustering.
}

\value{
A single value of \eqn{\gamma^c}
}

\references{
Ellison G./Glaeser, E. (1997): \dQuote{Geographic concentration in u.s. manufacturing industries: A dartboard approach}. In: \emph{Journal of Political Economy}, \bold{105}, 5, p. 889-927.

Farhauer, O./Kroell, A. (2014): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Nakamura R./Morrison Paul, C. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds): \emph{Handbook of Regional Growth and Development Theories}, p. 305-328.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{ellison.a}}, \code{\link{ellison.a2}}, \code{\link{ellison.c}}, \code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{locq}}, \code{\link{locq2}}, \code{\link{howard.cl}}, \code{\link{howard.xcl}}, \code{\link{howard.xcl2}}, \code{\link{litzenberger}}, \code{\link{litzenberger2}}
}

\examples{
# Example from Farhauer/Kroell (2014):
data(FK2014_EGK)
ellison.c2(FK2014_EGK$Beschaeftigte, FK2014_EGK$Branche, 
FK2014_EGK$Region, FK2014_EGK$BeschRegion)
# this may take a while
}