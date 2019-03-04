\name{betaconv.speed}
\alias{betaconv.speed}

\title{
Regional beta convergence: Convergence speed and half-life
}

\description{
This function calculates the beta convergence speed and half-life based on a given beta value and time interval.
}

\usage{
betaconv.speed(beta, tinterval, print.results = TRUE)
}


\arguments{
  \item{beta}{
Beta value
}
  \item{tinterval}{
Time interval (in time units, such as years)
}
  \item{print.results}{
Logical argument that indicates if the function shows the results or not
}

}
\details{
From the regional economic perspective (in particular the neoclassical growth theory), regional disparities are expected to decline. This \emph{convergence} can have different meanings: \emph{Sigma convergence} (\eqn{\sigma}) means a harmonization of regional economic output or income over time, while \emph{beta convergence} (\eqn{\beta}) means a decline of dispersion because poor regions have a stronger economic growth than rich regions (Capello/Nijkamp 2009). Regardless of the theoretical assumptions of a harmonization in reality, the related analytical framework allows to analyze both types of convergence for cross-sectional data (GDP p.c. or another economic variable, \eqn{y}, for \eqn{i} regions and two points in time, \eqn{t} and \eqn{t+T}), or one starting point (\eqn{t}) and the average growth within the following \eqn{n} years (\eqn{t+1, t+2, ..., t+n}), respectively. Beta convergence can be calculated either in a linearized OLS regression model or in a nonlinear regression model. When no other variables are integrated in this model, it is called \emph{absolute} beta convergence. Implementing other region-related variables (conditions) into the model leads to \emph{conditional} beta convergence. If there is beta convergence (\eqn{\beta < 0}), it is possible to calculate the \emph{speed of convergence}, \eqn{\lambda}, and the so-called \emph{Half-Life} \eqn{H}, while the latter is the time taken to reduce the disparities by one half (Allington/McCombie 2007, Goecke/Huether 2016). There is \emph{sigma convergence}, when the dispersion of the variable (\eqn{\sigma}), e.g. calculated as standard deviation or coefficient of variation, reduces from \eqn{t} to \eqn{t+T}. This can be measured using ANOVA for two years or trend regression with respect to several years (Furceri 2005, Goecke/Huether 2016). 

This function calculates the \emph{speed of convergence}, \eqn{\lambda}, and the \emph{Half-Life}, \eqn{H}, based on a given \eqn{\beta} value and time interval.
}

\value{
A \code{matrix} containing the following objects:
\item{Lambda }{Lambda value (convergence speed)}
\item{Half-Life }{Half-life values}
}

\references{
Allington, N. F. B./McCombie, J. S. L. (2007): \dQuote{Economic growth and beta-convergence in the East European Transition Economies}. In: Arestis, P./Baddely, M./McCombie, J. S. L. (eds.): \emph{Economic Growth. New Directions in Theory and Policy}. Cheltenham: Elgar. p. 200-222.

Capello, R./Nijkamp, P. (2009): \dQuote{Introduction: regional growth and development theories in the twenty-first century - recent theoretical advances and future challenges}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 1-16.

Dapena, A. D./Vazquez, E. F./Morollon, F. R. (2016): \dQuote{The role of spatial scale in regional convergence: the effect of MAUP in the estimation of beta-convergence equations}. In: \emph{The Annals of Regional Science}, \bold{56}, 2, p. 473-489.

Furceri, D. (2005): \dQuote{Beta and sigma-convergence: A mathematical relation of causality}. In: \emph{Economics Letters}, \bold{89}, 2, p. 212-215.

Goecke, H./Huether, M. (2016): \dQuote{Regional Convergence in Europe}. In: \emph{Intereconomics}, \bold{51}, 3, p. 165-171.

Young, A. T./Higgins, M. J./Levy, D. (2008): \dQuote{Sigma Convergence versus Beta Convergence: Evidence from U.S. County-Level Data}. In: \emph{Journal of Money, Credit and Banking}, \bold{40}, 5, p. 1083-1093.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{betaconv.nls}}, \code{\link{betaconv.ols}}, \code{\link{sigmaconv}}, \code{\link{sigmaconv.t}}, \code{\link{cv}}, \code{\link{sd2}}, \code{\link{var2}}
}

\examples{
speed <- betaconv.speed(-0.008070533, 1)
speed[1] # lambda
speed[2] # half-life
}