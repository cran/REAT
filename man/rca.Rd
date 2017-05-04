\name{rca}
\alias{rca}

\title{
Analysis of regional convergence
}
\description{
This function provides the analysis of absolute regional economic convergence (beta and sigma convergence) for cross-sectional data.
}
\usage{
rca(gdp1, time1, gdp2, time2, output = "all", sigma.measure = "cv", 
sigma.log = TRUE, sigma.norm = FALSE, sigma.weighting = NULL, digs = 5)
}


\arguments{
  \item{gdp1}{
A numeric vector containing the GDP per capita (or another economic variable) at time \emph{t}
}
  \item{time1}{
A single value of time \emph{t}, e.g. the initial year
}
  \item{gdp2}{
A numeric vector containing the GDP per capita (or another economic variable) at time \emph{t+1}
}
  \item{time2}{
A single value of time \emph{t+1}
}
  \item{output}{
argument that indicates the type of function output: if \code{output = "all"} (default), the function returns a \code{list} containing the results. If \code{output = "data"}, the function only returns the input variables and their transformations in a \code{data.frame}. If \code{output = "lm"}, an \code{lm} object of the (linearized) model is returned.
}
  \item{sigma.measure}{
argument that indicates how the sigma convergence should be measured. The default is \code{output = "cv"}, which means that a coefficient of variation is used. If \code{output = "sd"}, the standard deviation is used.
}
  \item{sigma.log}{
Logical argument. Per default (\code{sigma.log = TRUE}), also in the sigma convergence analysis, the economic variables are transformed by natural logarithm. If the original values should be used, state \code{sigma.log = FALSE} 
}
  \item{sigma.norm}{
Logical argument that indicates if a normalized coefficient of variation should be used instead
}
  \item{sigma.weighting}{
If the measure of statistical dispersion in the sigma convergence analysis (coefficient of variation or standard deviation) should be weighted, a weighting vector has to be stated 
}
  \item{digs}{
The number of digits for the resulting values (default: \code{digs = 5})
}
}

\details{
From the regional economic perspective (in particular the neoclassical growth theory), regional disparities are expected to decline. This \emph{convergence}
can have different meanings: \emph{Sigma convergence} (\eqn{\sigma}) means a harmonization of regional economic output or income over time, while \emph{beta convergence} (\eqn{\beta}) means a decline of dispersion because poor regions have a stronger economic growth than rich regions (Capello/Nijkamp 2009). Regardless of the theoretical assumptions of a harmonization in reality, the related analytical framework allows to analyze both types of convergence for cross-sectional data (GDP p.c. or another economic variable, \eqn{y}, for \eqn{i} regions and two points in time, \eqn{t} and \eqn{t+T}). Given two GDPs per capita or another economic variable, (absolute) beta convergence can be calculated as the "slope" of a linearized OLS regression model of \eqn{\ln \Delta y_{i,t+T}} against \eqn{\ln y_{i,t}}. If there is beta convergence (\eqn{-1 < \beta < 0}), it is possible to calculate the \emph{speed of convergence}, \eqn{\lambda}, and the so-called \emph{Half-Life} \eqn{H}, while the latter is the time taken to reduce the disparities by one half (Allington/McCombie 2007). There is \emph{sigma convergence}, when the dispersion of the variable (\eqn{\sigma}), e.g. calculated as standard deviation or coefficient of variation, reduces from \eqn{t} to \eqn{t+T} (Furceri 2005). 

This function needs two vectors (GDP p.c. or another economic variable, \eqn{y}, for \eqn{i} regions) and the related two points in time (\eqn{t} and \eqn{t+T}).  If \code{output = "all"}, it returns the estimation results of beta convergence and, if \eqn{-1 < \beta < 0}, also the calculations of \eqn{\lambda} and \eqn{H} related to \eqn{\beta}. The \emph{sigma convergence} is operationalized as the difference between the dispersions of the regared variable (ln-transformed if \code{sigma.log = TRUE}): \eqn{\sigma_t - \sigma_{t+T}}. If this value is positive, there is \emph{sigma convergence} with respect to these points in time. The dispersions can be calculated as (weighted or non-weighted, standardized or non-standardized) standard deviation or coefficient of variation (see the function \code{cv}), to be stated by the function parameters \code{sigma.measure}, \code{sigma.norm} and \code{sigma.weighting}. State \code{output = "lm"} for the underlying regression model (\code{lm} object) only or \code{output = "data"} for the transformed dataset. As yet, the function only allows absolute beta convergence.
}

\value{
If \code{output = "all"}: a \code{list} containing the items
\item{constant }{The constant in the beta convergence OLS model}
\item{beta }{The "slope" of the OLS model (beta convergence)}
\item{tinterval }{Time interval between t and t+T, in units of time}
\item{lambda }{Lambda, the speed of convergence (NA in absence of beta convergence)}
\item{halflife }{H, the half-life value (NA in absence of beta convergence)}
\item{r.squared }{R-Squared of the OLS model}
\item{N }{Number of regarded regions}
\item{sigma }{Difference in dispersion parameter between t and t+T}

If \code{output = "data"}: a \code{data.frame} containing the columns
\item{gdp1 }{the input GDP per capita (or another economic variable) at time t}
\item{gdp2 }{the input GDP per capita (or another economic variable) at time t+T}
\item{diff }{the absolute difference between gdp2 and gdp1 ((t+T) - t)}
\item{diff }{the relative difference between gdp2 and gdp1 ((t+T) - t)}
\item{ln_growth }{natural logarithm of the growth}
\item{ln_initial }{natural logarithm of the initial value at time t}

If \code{output = "lm"}: A \code{lm} object of the estimated OLS model
}

\references{
Allington, N. F. B./McCombie, J. S. L. (2007): \dQuote{Economic growth and beta-convergence in the East European Transition Economies}. In: Arestis, P./Baddely, M./McCombie, J. S. L. (eds.): \emph{Economic Growth. New Directions in Theory and Policy}. Cheltenham: Elgar. p. 200-222.

Capello, R./Nijkamp, P. (2009): \dQuote{Introduction: regional growth and development theories in the twenty-first century - recent theoretical advances and future challenges}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 1-16.

Dapena, A. D./Vazquez, E. F./Morollon, F. R. (2016): \dQuote{The role of spatial scale in regional convergence: the effect of MAUP in the estimation of beta-convergence equations}. In: \emph{The Annals of Regional Science}, \bold{56}, 2, p. 473-489.

Furceri, D. (2005): \dQuote{Beta and sigma-convergence: A mathematical relation of causality}. In: \emph{Economics Letters}, \bold{89}, 2, p. 212-215.

Young, A. T./Higgins, M. J./Levy, D. (2008): \dQuote{Sigma Convergence versus Beta Convergence: Evidence from U.S. County-Level Data}. In: \emph{Journal of Money, Credit and Banking}, \bold{40}, 5, p. 1083-1093.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{cv}}
}

\examples{ 
# Regional disparities / beta and sigma convergence in Germany
data(G.counties.gdp)
# GDP per capita for German counties (Landkreise)
rca (G.counties.gdp$gdppc2005, 2005, G.counties.gdp$gdppc2009, 2009, digs=5)
# returns a list
convergence <- rca (G.counties.gdp$gdppc2005, 2005, G.counties.gdp$gdppc2009, 2009, digs=5)
beta <- convergence$beta
# Beta convergence value
}