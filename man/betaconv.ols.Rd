\name{betaconv.ols}
\alias{betaconv.ols}

\title{
Analysis of regional beta convergence using OLS regression
}

\description{
This function provides the analysis of absolute and conditional regional economic beta convergence for cross-sectional data using ordinary least squares (OLS) technique.
}

\usage{
betaconv.ols(gdp1, time1, gdp2, time2, conditions = NULL, beta.plot = FALSE, 
beta.plotPSize = 1, beta.plotPCol = "black", beta.plotLine = FALSE, 
beta.plotLineCol = "red", beta.plotX = "Ln (initial)", beta.plotY = "Ln (growth)", 
beta.plotTitle = "Beta convergence", beta.bgCol = "gray95", beta.bgrid = TRUE,
beta.bgridCol = "white", beta.bgridSize = 2, beta.bgridType = "solid", 
print.results = FALSE)
}

\arguments{
  \item{gdp1}{
A numeric vector containing the GDP per capita (or another economic variable) at time \emph{t}
}
  \item{time1}{
A single value of time \emph{t} (= the initial year)
}
  \item{gdp2}{
A numeric vector containing the GDP per capita (or another economic variable) at time \emph{t+1} or a data frame containing the GDPs per capita (or another economic variable) at time \emph{t+1, t+2, t+3, ..., t+n}
}
  \item{time2}{
A single value of time \emph{t+1} or \emph{t_n}, respectively
}
  \item{conditions}{
A data frame containing the conditions for conditional beta convergence
}
  \item{beta.plot}{
Boolean argument that indicates if a plot of beta convergence has to be created
}
  \item{beta.plotPSize}{
If \code{beta.plot = TRUE}: Point size in the beta convergence plot
}
  \item{beta.plotPCol}{
If \code{beta.plot = TRUE}: Point color in the beta convergence plot
}
  \item{beta.plotLine}{
If \code{beta.plot = TRUE}: Logical argument that indicates if a regression line has to be added to the plot
}
  \item{beta.plotLineCol}{
If \code{beta.plot = TRUE} and \code{beta.plotLine = TRUE}: Line color of regression line
}
  \item{beta.plotX}{
If \code{beta.plot = TRUE}: Name of the X axis
}
  \item{beta.plotY}{
If \code{beta.plot = TRUE}: Name of the Y axis
}
  \item{beta.plotTitle}{
If \code{beta.plot = TRUE}: Plot title
}
  \item{beta.bgCol}{
If \code{beta.plot = TRUE}: Plot background color
}
  \item{beta.bgrid}{
If \code{beta.plot = TRUE}: Logical argument that indicates if the plot contains a grid
}
  \item{beta.bgridCol}{
If \code{beta.plot = TRUE} and \code{beta.bgrid = TRUE}: Color of the grid 
}
  \item{beta.bgridSize}{
If \code{beta.plot = TRUE} and \code{beta.bgrid = TRUE}: Size of the grid
}
  \item{beta.bgridType}{
If \code{beta.plot = TRUE} and \code{beta.bgrid = TRUE}: Type of the grid
}
  \item{print.results}{
Logical argument that indicates if the function shows the results or not
}

}
\details{
From the regional economic perspective (in particular the neoclassical growth theory), regional disparities are expected to decline. This \emph{convergence} can have different meanings: \emph{Sigma convergence} (\eqn{\sigma}) means a harmonization of regional economic output or income over time, while \emph{beta convergence} (\eqn{\beta}) means a decline of dispersion because poor regions have a stronger economic growth than rich regions (Capello/Nijkamp 2009). Regardless of the theoretical assumptions of a harmonization in reality, the related analytical framework allows to analyze both types of convergence for cross-sectional data (GDP p.c. or another economic variable, \eqn{y}, for \eqn{i} regions and two points in time, \eqn{t} and \eqn{t+T}), or one starting point (\eqn{t}) and the average growth within the following \eqn{n} years (\eqn{t+1, t+2, ..., t+n}), respectively. Beta convergence can be calculated either in a linearized OLS regression model or in a nonlinear regression model. When no other variables are integrated in this model, it is called \emph{absolute} beta convergence. Implementing other region-related variables (conditions) into the model leads to \emph{conditional} beta convergence. If there is beta convergence (\eqn{\beta < 0}), it is possible to calculate the \emph{speed of convergence}, \eqn{\lambda}, and the so-called \emph{Half-Life} \eqn{H}, while the latter is the time taken to reduce the disparities by one half (Allington/McCombie 2007, Goecke/Huether 2016). There is \emph{sigma convergence}, when the dispersion of the variable (\eqn{\sigma}), e.g. calculated as standard deviation or coefficient of variation, reduces from \eqn{t} to \eqn{t+T}. This can be measured using ANOVA for two years or trend regression with respect to several years (Furceri 2005, Goecke/Huether 2016). 

This function calculates absolute and/or conditional beta convergence using ordinary least squares regression (OLS) for estimation. It needs at least two vectors (GDP p.c. or another economic variable, \eqn{y}, for \eqn{i} regions) and the related two points in time (\eqn{t} and \eqn{t+T}). If the beta coefficient is negative (using OLS) or positive (using NLS), there is beta convergence.
}

\value{
A \code{list} containing the following objects:
\item{regdata }{A data frame containing the regression data, including the \eqn{ln}-transformed economic variables}
\item{abeta }{A list containing the estimates of the absolute beta convergence regression model, including lambda and half-life}
\item{cbeta }{If conditions are stated: a list containing the estimates of the conditional beta convergence regression model, including lambda and half-life}
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
\code{\link{rca}}, \code{\link{betaconv.nls}}, \code{\link{betaconv.speed}}, \code{\link{sigmaconv}}, \code{\link{sigmaconv.t}}, \code{\link{cv}}, \code{\link{sd2}}, \code{\link{var2}}
}

\examples{
data (G.counties.gdp)

betaconv.ols (G.counties.gdp$gdppc2010, 2010, G.counties.gdp$gdppc2011, 2011, 
conditions = NULL, print.results = TRUE)
# Two years, no conditions (Absolute beta convergence)

regionaldummies <- to.dummy(G.counties.gdp$regional)
# Creating dummy variables for West/East
G.counties.gdp$West <- regionaldummies[,2]
G.counties.gdp$East <- regionaldummies[,1]
# Adding dummy variables to data

betaconv.ols (G.counties.gdp$gdppc2010, 2010, G.counties.gdp$gdppc2011, 2011, 
conditions = G.counties.gdp[c(70,71)], print.results = TRUE)
# Two years, with condition (dummy for West/East)
# (Absolute and conditional beta convergence)

betaconverg1 <- betaconv.ols (G.counties.gdp$gdppc2010, 2010, G.counties.gdp$gdppc2011, 2011,
conditions = G.counties.gdp[c(70,71)], print.results = TRUE)
# Store results in object
betaconverg1$cbeta$estimates
# Addressing estimates for the conditional beta model


betaconv.ols (G.counties.gdp$gdppc2010, 2010, G.counties.gdp[65:66], 2012, 
conditions = NULL, print.results = TRUE)
# Three years (2010-2012), no conditions (Absolute beta convergence)

betaconv.ols (G.counties.gdp$gdppc2010, 2010, G.counties.gdp[65:66], 2012, 
conditions = G.counties.gdp[c(70,71)], print.results = TRUE)
# Three years (2010-2012), with conditions (Absolute and conditional beta convergence)

betaconverg2 <- betaconv.ols (G.counties.gdp$gdppc2010, 2010, G.counties.gdp[65:66],
2012, conditions = G.counties.gdp[c(70,71)], print.results = TRUE)
# Store results in object
betaconverg2$cbeta$estimates
# Addressing estimates for the conditional beta model
}