\name{gini}
\alias{gini}

\title{
Gini coefficient
}

\description{
Calculating the Gini coefficient of inequality (or concentration), standardized and non-standardized, and optionally plotting the Lorenz curve
}

\usage{
gini(x, coefnorm = FALSE, weighting = NULL, na.rm = TRUE, lc = FALSE, 
lcx = "\% of objects", lcy = "\% of regarded variable", 
lctitle = "Lorenz curve", le.col = "blue", lc.col = "black",
lsize = 1, ltype = "solid",
bg.col = "gray95", bgrid = TRUE, bgrid.col = "white", 
bgrid.size = 2, bgrid.type = "solid",
lcg = FALSE, lcgn = FALSE, lcg.caption = NULL, 
lcg.lab.x = 0, lcg.lab.y = 1, add.lc = FALSE) 
}

\arguments{
  \item{x}{
A numeric vector (e.g. dataset of household income, sales turnover or supply)
}
  \item{coefnorm}{
logical argument that indicates if the function output is the non-standardized or the standardized Gini coefficient (default: \code{coefnorm = FALSE}, that means the non-standardized Gini coefficient is returned)
}
  \item{weighting}{
A numeric vector containing the weighting data (e.g. size of income classes when calculating a Gini coefficient for aggregated income data)
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
  \item{lc}{
logical argument that indicates if the Lorenz curve is plotted additionally (default: \code{lc = FALSE}, so no Lorenz curve is displayed)
}
  \item{lcx}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{lcx} defines the x axis label
}
  \item{lcy}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{lcy} defines the y axis label
}
  \item{lctitle}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{lctitle} defines the overall title of the Lorenz curve plot 
}
  \item{le.col}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{le.col} defines the color of the diagonale (line of equality) 
}
  \item{lc.col}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{lc.col} defines the color of the Lorenz curve 
}
  \item{lsize}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{lsize} defines the size of the lines (default: 1) 
}
  \item{ltype}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{ltype} defines the type of the lines (default: \code{"solid"}) 
}
  \item{bg.col}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{bg.col} defines the background color of the plot (default: \code{"gray95"})
}
  \item{bgrid}{
if \code{lc = TRUE} (plot of Lorenz curve), the logical argument \code{bgrid} defines if a grid is shown in the plot
}
  \item{bgrid.col}{
if \code{lc = TRUE} (plot of Lorenz curve) and \code{bgrid = TRUE} (background grid), \code{bgrid.col} defines the color of the background grid (default: "white")
}
  \item{bgrid.size}{
if \code{lc = TRUE} (plot of Lorenz curve) and \code{bgrid = TRUE} (background grid), \code{bgrid.size} defines the size of the background grid (default: 2)
}
  \item{bgrid.type}{
if \code{lc = TRUE} (plot of Lorenz curve) and \code{bgrid = TRUE} (background grid), \code{bgrid.type} defines the type of lines of the background grid (default: \code{"solid"})
}
  \item{lcg}{
if \code{lc = TRUE} (plot of Lorenz curve), the logical argument \code{lcg} defines if the non-standardized Gini coefficient is displayed in the Lorenz curve plot
}
  \item{lcgn}{
if \code{lc = TRUE} (plot of Lorenz curve), the logical argument \code{lcgn} defines if the standardized Gini coefficient is displayed in the Lorenz curve plot
}
  \item{lcg.caption}{
if \code{lcg = TRUE} (displaying the Gini coefficient in the plot), \code{lcg.caption} specifies the caption above the coefficients
}
  \item{lcg.lab.x}{
if \code{lcg = TRUE} (displaying the Gini coefficient in the plot), \code{lcg.lab.x} specifies the x coordinate of the label
}
  \item{lcg.lab.y}{
if \code{lcg = TRUE} (displaying the Gini coefficient in the plot), \code{lcg.lab.y} specifies the y coordinate of the label
}
  \item{add.lc}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{add.lc} specifies if a new Lorenz curve is plotted (\code{add.lc = "FALSE"}) or the plot is added to an existing Lorenz curve plot (\code{add.lc = "TRUE"})
}
}

\details{
The \emph{Gini coefficient} (Gini 1912) is a popular measure of statistical dispersion, especially used for analyzing inequality or concentration. The \emph{Lorenz curve} (Lorenz 1905), though developed independently, can be regarded as a graphical representation of the degree of inequality/concentration calculated by the \emph{Gini coefficient} (\eqn{G}) and can also be used for additional interpretations of it. In an economic-geographical context, these methods are frequently used to analyse the concentration/inequality of income or wealth within countries (Aoyama et al. 2011). Other areas of application are analyzing regional disparities (Lessmann 2005, Nakamura 2008) and concentration in markets (sales turnover of competing firms) which makes Gini and Lorenz part of economic statistics in general (Doersam 2004, Roberts 2014).

The \emph{Gini coefficient} (\eqn{G}) varies between 0 (no inequality/concentration) and 1 (complete inequality/concentration). The \emph{Lorenz curve} displays the deviations of the empirical distribution from a perfectly equal distribution as the difference between two graphs (the distribution curve and a diagonal line of perfect equality). This function calculates \eqn{G} and plots the \emph{Lorenz curve} optionally. As there are several ways to calculate the \emph{Gini coefficient}, this function uses the formula given in Doersam (2004). Because the maximum of \eqn{G} is not equal to 1, also a standardized coefficient (\eqn{G*}) with a maximum equal to 1 can be calculated alternatively. If a Gini coefficient for aggregated data (e.g. income classes with averaged incomes) or the Gini coefficient has to be weighted, use a \code{weighting} vector (e.g. size of the income classes).      
}

\value{
A single numeric value of the \emph{Gini coefficient} (\eqn{0 < G < 1}) or the \emph{standardized Gini coefficient} (\eqn{0 < G* < 1}) and, optionally, a plot of the \emph{Lorenz curve}.
}

\references{
Aoyama, Y./Murphy, J. T./Hanson, S. (2011): \dQuote{Key Concepts in Economic Geography}. London : SAGE.

Bahrenberg, G./Giese, E./Mevenkamp, N./Nipper, J. (2010): \dQuote{Statistische Methoden in der Geographie. Band 1: Univariate und bivariate Statistik}. Stuttgart: Borntraeger.

Cerlani, L./Verme, P. (2012): \dQuote{The origins of the Gini index: extracts from Variabilita e Mutabilita (1912) by Corrado Gini}. In: \emph{The Journal of Economic Inequality}, \bold{10}, 3, p. 421-443. 

Doersam, P. (2004): \dQuote{Wirtschaftsstatistik anschaulich dargestellt}. Heidenau : PD-Verlag.

Gini, C. (1912): \dQuote{Variabilita e Mutabilita}. Contributo allo Studio delle Distribuzioni e delle Relazioni
Statistiche. Bologna : Cuppini.

Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.ifo.de/DocDL/ifodb_2005_3_25-33.pdf}.

Lorenz, M. O. (1905): \dQuote{Methods of Measuring the Concentration of Wealth}. In: \emph{Publications of the American Statistical Association}, \bold{9}, 70, p. 209-219.

Nakamura, R. (2008): \dQuote{Agglomeration Effects on Regional Economic Disparities: A Comparison between the UK and Japan}. In: \emph{Urban Studies}, \bold{45}, 9, p. 1947-1971.

Roberts, T. (2014): \dQuote{When Bigger Is Better: A Critique of the Herfindahl-Hirschman Index's Use to Evaluate Mergers in Network Industries}. In: \emph{Pace Law Review}, \bold{34}, 2, p. 894-946.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{cv}}, \code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{herf}}, \code{\link{hoover}}
}

\examples{
# Market concentration (example from Doersam 2004):
sales <- c(20,50,20,10)
# sales turnover of four car manufacturing companies
gini (sales, lc = TRUE, lcx = "percentage of companies", lcy = "percentrage of sales", 
lctitle = "Lorenz curve of sales", lcg = TRUE, lcgn = TRUE)
# returs the non-standardized Gini coefficient (0.3) and 
# plots the Lorenz curve with user-defined title and labels 
gini (sales, coefnorm = TRUE)
# returns the standardized Gini coefficient (0.4)

# Income classes (example from Doersam 2004):
income <- c(500, 1500, 2500, 4000, 7500, 15000)
# average income of 6 income classes
sizeofclass <- c(1000, 1200, 1600, 400, 200, 600)
# size of income classes
gini (income, weighting = sizeofclass)
# returns the non-standardized Gini coefficient (0.5278)

# Market concentration in automotive industry
data(Automotive)
gini(Automotive$Turnover2008, lsize=1, lc=TRUE, le.col = "black", 
lc.col = "orange", lcx = "Shares of companies", lcy = "Shares of turnover / cars", 
lctitle = "Automotive industry: market concentration", 
lcg = TRUE, lcgn = TRUE, lcg.caption = "Turnover 2008:", lcg.lab.x = 0, lcg.lab.y = 1)
# Gini coefficient and Lorenz curve for turnover 2008
gini(Automotive$Turnover2013, lsize=1, lc = TRUE, add.lc = TRUE, lc.col = "red", 
lcg = TRUE, lcgn = TRUE, lcg.caption = "Turnover 2013:", lcg.lab.x = 0, lcg.lab.y = 0.85)
# Adding Gini coefficient and Lorenz curve for turnover 2013
gini(Automotive$Quantity2014_car, lsize=1, lc = TRUE, add.lc = TRUE, lc.col = "blue", 
lcg = TRUE, lcgn = TRUE, lcg.caption = "Cars 2014:", lcg.lab.x = 0, lcg.lab.y = 0.7)
# Adding Gini coefficient and Lorenz curve for cars 2014

# Regional disparities in Germany:
gdp <- c(460.69, 549.19, 124.16, 65.29, 31.59, 109.27, 263.44, 39.87, 258.53, 
645.59, 131.95, 35.03, 112.66, 56.22, 85.61, 56.81)
# GDP of german regions (Bundeslaender) 2015 (in billion EUR)
gini(gdp)
# returs the non-standardized Gini coefficient (0.5009)
}
