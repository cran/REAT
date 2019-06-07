\name{ellison.c}
\alias{ellison.c}

\title{
Ellison-Glaeser Coagglomeration Index
}

\description{
Calculating the Coagglomeration Index by Ellison and Glaeser for one set of \eqn{U} industries
}

\usage{
ellison.c(e_ik, industry, region, e_j = NULL, c.industries = NULL)
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
  \item{c.industries}{
optional: a vector containing the regarded \eqn{U} industries (where \eqn{U \le I})
}
}

\details{
The Ellison-Glaeser Coagglomeration Index is not standardized. A value of \eqn{\gamma_c = 0} indicates a spatial distribution of firms equal to a dartboard approach. Values below zero indicate spatial dispersion, values greater than zero indicate clustering.
}

\value{
A single value of \eqn{\gamma_c}
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
\code{\link{ellison.a}}, \code{\link{ellison.a2}}, \code{\link{ellison.c2}}, \code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{locq}}, \code{\link{locq2}}, \code{\link{howard.cl}}, \code{\link{howard.xcl}}, \code{\link{howard.xcl2}}, \code{\link{litzenberger}}, \code{\link{litzenberger2}}
}

\examples{
# Example from Farhauer/Kroell (2014):
data(FK2014_EGC)

ellison.c(FK2014_EGC$emp_firm, FK2014_EGC$industry, 
FK2014_EGC$region, FK2014_EGC$emp_region)
}