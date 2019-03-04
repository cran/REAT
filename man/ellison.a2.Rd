\name{ellison.a2}
\alias{ellison.a2}

\title{
Ellison-Glaeser Agglomeration Index
}

\description{
Calculating the Agglomeration Index by Ellison and Glaeser for a given number of \eqn{I} industries
}

\usage{
ellison.a2(e_ik, industry, region, print.results = TRUE)
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
  \item{print.results}{
logical argument that indicates whether the function prints the results or not (only for internal use)
}
}

\details{
The Ellison-Glaeser Agglomeration Index is not standardized. A value of \eqn{\gamma_i = 0} indicates a spatial distribution of firms equal to a dartboard approach. Values below zero indicate spatial dispersion, values greater than zero indicate clustering.
}

\value{
A matrix with five columns (\eqn{\gamma_i}, \eqn{G_i}, \eqn{z_{G_i}}, \eqn{K_i} and \eqn{HHI_i}) and \eqn{I} rows (one for each industry).
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
\code{\link{ellison.a}}, \code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{locq}}, \code{\link{locq2}}, \code{\link{howard.cl}}, \code{\link{howard.xcl}}, \code{\link{howard.xcl2}}, \code{\link{litzenberger}}, \code{\link{litzenberger2}}
}

\examples{
# Example from Farhauer/Kroell (2014):
j <- c("Wien", "Wien", "Wien", "Wien", "Wien", "Linz", 
"Linz", "Linz", "Linz", "Graz")
E_ik <- c(200,650,12000,100,50,16000,13000,1500,1500,25000)
E_j <- c(500000,400000,100000)
ellison.a(E_ik, E_j, j)
# 0.05990628
}