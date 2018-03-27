\name{sigmaconv}
\alias{sigmaconv}

\title{
Analysis of regional sigma convergence for two years using ANOVA
}

\description{
This function provides the analysis of regional economic sigma convergence (decline of deviation) for two years using ANOVA (Analysis of Variance)
}

\usage{
sigmaconv(gdp1, time1, gdp2, time2, sigma.measure = "sd", 
sigma.log = TRUE, sigma.weighting = NULL, sigma.norm = FALSE, 
sigma.issample = FALSE, output.results = FALSE)
}

\arguments{
  \item{gdp1}{
A numeric vector containing the GDP per capita (or another economic variable) at time \emph{t}
}
  \item{time1}{
A single value of time \emph{t} (= the initial year)
}
  \item{gdp2}{
A numeric vector containing the GDP per capita (or another economic variable) at time \emph{t+1}
}
  \item{time2}{
A single value of time \emph{t+1}
}
  \item{sigma.measure}{
argument that indicates how the sigma convergence should be measured. The default is \code{output = "sd"}, which means that the standard deviation is used. If \code{output = "var"} or \code{output = "cv"}, the variance or the coefficient of variation is used, respectively.
}
  \item{sigma.log}{
Logical argument. Per default (\code{sigma.log = TRUE}), also in the sigma convergence analysis, the economic variables are transformed by natural logarithm. If the original values should be used, state \code{sigma.log = FALSE} 
}
  \item{sigma.weighting}{
If the measure of statistical dispersion in the sigma convergence analysis (coefficient of variation or standard deviation) should be weighted, a weighting vector has to be stated 
}
  \item{sigma.norm}{
Logical argument that indicates if a normalized coefficient of variation should be used instead
}
  \item{sigma.issample}{
logical argument that indicates if the dataset is a sample or the population (default: \code{is.sample = FALSE}, so the denominator of variance is \eqn{n}) 
}
  \item{output.results}{
Logical argument that indicates if the function shows the results or not
}
}

\details{
From the regional economic perspective (in particular the neoclassical growth theory), regional disparities are expected to decline. This \emph{convergence} can have different meanings: \emph{Sigma convergence} (\eqn{\sigma}) means a harmonization of regional economic output or income over time, while \emph{beta convergence} (\eqn{\beta}) means a decline of dispersion because poor regions have a stronger economic growth than rich regions (Capello/Nijkamp 2009). Regardless of the theoretical assumptions of a harmonization in reality, the related analytical framework allows to analyze both types of convergence for cross-sectional data (GDP p.c. or another economic variable, \eqn{y}, for \eqn{i} regions and two points in time, \eqn{t} and \eqn{t+T}), or one starting point (\eqn{t}) and the average growth within the following \eqn{n} years (\eqn{t+1, t+2, ..., t+n}), respectively. Beta convergence can be calculated either in a linearized OLS regression model or in a nonlinear regression model. When no other variables are integrated in this model, it is called \emph{absolute} beta convergence. Implementing other region-related variables (conditions) into the model leads to \emph{conditional} beta convergence. If there is beta convergence (\eqn{\beta < 0}), it is possible to calculate the \emph{speed of convergence}, \eqn{\lambda}, and the so-called \emph{Half-Life} \eqn{H}, while the latter is the time taken to reduce the disparities by one half (Allington/McCombie 2007, Goecke/Huether 2016). There is \emph{sigma convergence}, when the dispersion of the variable (\eqn{\sigma}), e.g. calculated as standard deviation or coefficient of variation, reduces from \eqn{t} to \eqn{t+T}. This can be measured using ANOVA for two years or trend regression with respect to several years (Furceri 2005, Goecke/Huether 2016). 

This function calculates the standard deviation (or variance, coefficient of variation) for the GDP per capita (or another economic variable) for both years and executes an analysis of variance (ANOVA) for these deviation measures (year 1 divided by year 2, F-statistic). If \eqn{\sigma_t1/\sigma_t2 > 0}, there is sigma convergence.
}

\value{
Returns a \code{matrix} containing the standard deviations, their quotient and the results of the significance test (F-statistic).
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
\code{\link{rca}}, \code{\link{sigmaconv.t}}, \code{\link{betaconv.nls}}, \code{\link{betaconv.speed}}, \code{\link{cv}}, \code{\link{sd2}}, \code{\link{var2}}
}

\examples{
data(G.counties.gdp)
# Loading GDP data for Germany (counties = Landkreise)

sigmaconv (G.counties.gdp$gdppc2010, 2010, G.counties.gdp$gdppc2011, 2011, 
sigma.measure = "cv", output.results =  TRUE)
# Using the coefficient of variation

sigmaconv (G.counties.gdp$gdppc2010, 2010, G.counties.gdp$gdppc2011, 2011, 
sigma.log = TRUE, output.results =  TRUE)
# Using the standard deviation with logged GDP per capita
}