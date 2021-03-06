\name{rca}
\alias{rca}

\title{
Analysis of regional beta and sigma convergence
}

\description{
This function provides the analysis of absolute and conditional regional economic beta convergence and sigma convergence for cross-sectional data. Beta convergence can be estimated using an OLS or NLS technique. Sigma convergence can be analyzed using ANOVA or trend regression.
}

\usage{
rca(gdp1, time1, gdp2, time2, 
conditions = NULL, conditions.formula = NULL, conditions.startval = NULL, 
beta.estimate = "ols", beta.plot = FALSE, beta.plotPSize = 1, beta.plotPCol = "black", 
beta.plotLine = FALSE, beta.plotLineCol = "red", beta.plotX = "Ln (initial)", 
beta.plotY = "Ln (growth)", beta.plotTitle = "Beta convergence", beta.bgCol = "gray95", 
beta.bgrid = TRUE, beta.bgridCol = "white", beta.bgridSize = 2, beta.bgridType = "solid", 
sigma.type = "anova", sigma.measure = "sd", sigma.log = TRUE, sigma.weighting = NULL, 
sigma.issample = FALSE, sigma.plot = FALSE, sigma.plotLSize = 1, 
sigma.plotLineCol = "black", sigma.plotRLine = FALSE, sigma.plotRLineCol = "blue", 
sigma.Ymin = 0, sigma.plotX = "Time", sigma.plotY = "Variation", 
sigma.plotTitle = "Sigma convergence", sigma.bgCol = "gray95", sigma.bgrid = TRUE, 
sigma.bgridCol = "white", sigma.bgridSize = 2, sigma.bgridType = "solid")
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
  \item{conditions.formula}{
If \code{beta.estimate = "nls"}: A formula for the functional linkage of the conditions in the case of conditional beta convergence
}
  \item{conditions.startval}{
If \code{beta.estimate = "nls"}: Starting values for the parameters of the conditions in the case of conditional beta convergence
}
  \item{beta.estimate}{
Beta estimate via ordinary least squares (OLS) or nonlinear least squares (NLS). Default: \code{beta.estimate = "ols"}
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
  \item{sigma.type}{
Estimating sigma convergence via ANOVA (two years) or trend regression (more than two years). Default: \code{sigma.type = "anova"}
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
  \item{sigma.issample}{
Logical argument that indicates if the dataset is a sample or the population (default: \code{is.sample = FALSE}, so the denominator of variance is \eqn{n}) 
}
  \item{sigma.plot}{
Logical argument that indicates if a plot of sigma convergence has to be created
}
  \item{sigma.plotLSize}{
If \code{sigma.plot = TRUE}: Line size of the sigma convergence plot
}
  \item{sigma.plotLineCol}{
If \code{sigma.plot = TRUE}: Line color of the sigma convergence plot
}
  \item{sigma.plotRLine}{
If \code{sigma.plot = TRUE}: Logical argument that indicates if a regression line has to be added to the plot
}
  \item{sigma.plotRLineCol}{
If \code{sigma.plot = TRUE} and \code{sigma.plotRLine = TRUE}: Color of the regression line
}
  \item{sigma.Ymin}{
If \code{sigma.plot = TRUE}: start value of the Y axis in the plot
}
  \item{sigma.plotX}{
If \code{sigma.plot = TRUE}: Name of the X axis
}
  \item{sigma.plotY}{
If \code{sigma.plot = TRUE}: Name of the Y axis
}
  \item{sigma.plotTitle}{
If \code{sigma.plot = TRUE}: Title of the plot
}
  \item{sigma.bgCol}{
If \code{sigma.plot = TRUE}: Plot background color
}
  \item{sigma.bgrid}{
If \code{sigma.plot = TRUE}: Logical argument that indicates if the plot contains a grid
}
  \item{sigma.bgridCol}{
If \code{sigma.plot = TRUE} and \code{sigma.bgrid = TRUE}: Color of the grid
}
  \item{sigma.bgridSize}{
If \code{sigma.plot = TRUE} and \code{sigma.bgrid = TRUE}: Size of the grid
}
  \item{sigma.bgridType}{
If \code{sigma.plot = TRUE} and \code{sigma.bgrid = TRUE}: Type of the grid
}

}
\details{
From the regional economic perspective (in particular the neoclassical growth theory), regional disparities are expected to decline. This \emph{convergence} can have different meanings: \emph{Sigma convergence} (\eqn{\sigma}) means a harmonization of regional economic output or income over time, while \emph{beta convergence} (\eqn{\beta}) means a decline of dispersion because poor regions have a stronger economic growth than rich regions (Capello/Nijkamp 2009). Regardless of the theoretical assumptions of a harmonization in reality, the related analytical framework allows to analyze both types of convergence for cross-sectional data (GDP p.c. or another economic variable, \eqn{y}, for \eqn{i} regions and two points in time, \eqn{t} and \eqn{t+T}), or one starting point (\eqn{t}) and the average growth within the following \eqn{n} years (\eqn{t+1, t+2, ..., t+n}), respectively. Beta convergence can be calculated either in a linearized OLS regression model or in a nonlinear regression model. When no other variables are integrated in this model, it is called \emph{absolute} beta convergence. Implementing other region-related variables (conditions) into the model leads to \emph{conditional} beta convergence. If there is beta convergence (\eqn{\beta < 0}), it is possible to calculate the \emph{speed of convergence}, \eqn{\lambda}, and the so-called \emph{Half-Life} \eqn{H}, while the latter is the time taken to reduce the disparities by one half (Allington/McCombie 2007, Goecke/Huether 2016). There is \emph{sigma convergence}, when the dispersion of the variable (\eqn{\sigma}), e.g. calculated as standard deviation or coefficient of variation, reduces from \eqn{t} to \eqn{t+T}. This can be measured using ANOVA for two years or trend regression with respect to several years (Furceri 2005, Goecke/Huether 2016). 

The \code{rca} function is a wrapper for the functions \code{betaconv.ols}, \code{betaconv.nls}, \code{sigmaconv} and \code{sigmaconv.t}. This function calculates (absolute and/or conditional) beta convergence and sigma convergence. Regional disparities are measured by the standard deviation (or variance, coefficient of variation) for all GDPs per capita (or another economic variable) for the given years. Beta convergence is estimated either using ordinary least squares (OLS) or nonlinear least squares (NLS). If the beta coefficient is negative (using OLS) or positive (using NLS), there is beta convergence. Sigma convergence is analyzed either using an analysis of variance (ANOVA) for these deviation measures (year 1 divided by year 2, F-statistic) or a trend regression (F-statistic, t-statistic). In the former case, if \eqn{\sigma_t1/\sigma_t2 > 0}, there is sigma convergence. In the latter case, if the slope of the trend regression is negative, there is sigma convergence.
}

\value{
A \code{list} containing the following objects:

\item{betaconv }{A list containing the following objects:}
\item{regdata }{A data frame containing the regression data, including the \eqn{ln}-transformed economic variables}
\item{tinterval }{The time interval}
\item{abeta }{A list containing the estimates of the absolute beta convergence regression model, including lambda and half-life}
\item{cbeta }{If conditions are stated: a list containing the estimates of the conditional beta convergence regression model, including lambda and half-life}

\item{sigmaconv }{A list containing the following objects:}
\item{sigmaconv }{A matrix containing either the standard deviations, their quotient and the results of the significance test (F-statistic) or the results of trend regression}
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
\code{\link{betaconv.ols}}, \code{\link{betaconv.nls}}, \code{\link{betaconv.speed}}, \code{\link{sigmaconv}}, \code{\link{sigmaconv.t}}, \code{\link{cv}}, \code{\link{sd2}}, \code{\link{var2}}
}

\examples{
data (G.counties.gdp)
# Loading GDP data for Germany (counties = Landkreise)

rca (G.counties.gdp$gdppc2010, 2010, G.counties.gdp$gdppc2011, 2011, 
conditions = NULL, beta.plot = TRUE)
# Two years, no conditions (Absolute beta convergence)


regionaldummies <- to.dummy(G.counties.gdp$regional)
# Creating dummy variables for West/East
G.counties.gdp$West <- regionaldummies[,2]
G.counties.gdp$East <- regionaldummies[,1]
# Adding dummy variables to data

rca (G.counties.gdp$gdppc2010, 2010, G.counties.gdp$gdppc2011, 2011, 
conditions = G.counties.gdp[c(70,71)])
# Two years, with conditions
# (Absolute and conditional beta convergence)

converg1 <- rca (G.counties.gdp$gdppc2010, 2010, G.counties.gdp$gdppc2011, 2011, 
conditions = G.counties.gdp[c(70,71)])
# Store results in object
converg1$betaconv$abeta
# Addressing estimates for the conditional beta model


rca (G.counties.gdp$gdppc2010, 2010, G.counties.gdp[65:68], 2014, conditions = NULL, 
sigma.type = "trend", beta.plot = TRUE, sigma.plot = TRUE)
# Five years, no conditions (Absolute beta convergence)
# with plots for both beta and sigma convergence
}