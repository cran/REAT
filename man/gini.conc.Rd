\name{gini.conc}
\alias{gini.conc}

\title{
Gini coefficient of spatial industry concentration
}

\description{
Calculating the Gini coefficient of spatial industry concentration based on regional industry data (normally employment data)
}

\usage{
gini.conc(e_ij, e_j, lc = FALSE, lcx = "\% of objects", 
lcy = "\% of regarded variable", lctitle = "Lorenz curve", 
le.col = "blue", lc.col = "black", lsize = 1, ltype = "solid",
bg.col = "gray95", bgrid = TRUE, bgrid.col = "white", 
bgrid.size = 2, bgrid.type = "solid", lcg = FALSE, lcgn = FALSE, 
lcg.caption = NULL, lcg.lab.x = 0, lcg.lab.y = 1, 
add.lc = FALSE, plot.lc = TRUE)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industry \eqn{i} in region \eqn{j}
}
  \item{e_j}{
a numeric vector with the employment in region \eqn{j}
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
  \item{plot.lc}{
logical argument that indicates if the Lorenz curve itself is plotted (if \code{plot.lc = FALSE}, only the line of equality is plotted))
}
}

\details{
The \emph{Gini coefficient of spatial industry concentration} (\eqn{G_{i}}) is a special spatial modification of the Gini coefficient of inequality (see the function \code{gini()}). It represents the rate of spatial concentration of the industry \eqn{i} referring to \eqn{j} regions (e.g. cities, counties, states). The coefficient \eqn{G_{i}} varies between 0 (perfect distribution, respectively no concentration) and 1 (complete concentration in one region). Optionally a Lorenz curve is plotted (if \code{lc = TRUE}).  
}

\value{
A single numeric value (\eqn{0 < G_{i} < 1})
}

\references{
Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Nakamura, R./Morrison Paul, C. J. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 305-328.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{gini.spec}}
}

\examples{
# Example from Farhauer/Kroell (2013):
E_ij <- c(500,500,1000,7000,1000)
# employment of the industry in five regions
E_j <- c(20000,15000,20000,40000,5000)
# employment in the five regions
gini.conc (E_ij, E_j)
# Returns the Gini coefficient of industry concentration (0.4068966)

data(G.regions.emp)
# Concentration of construction industry in Germany
# based on 16 German regions (Bundeslaender) for the year 2008
construction2008 <- G.regions.emp[(G.regions.emp$industry == "Baugewerbe (F)" | 
G.regions.emp$industry == "Insgesamt") & G.regions.emp$year == "2008",]
# only data for construction industry (Baugewerbe) and all-over (Insgesamt)
# for the 16 German regions in the year 2008
construction2008 <- construction2008[construction2008$region != "Insgesamt",]
# delete all-over data for all industries
gini.conc(construction2008[construction2008$industry=="Baugewerbe (F)",]$emp, 
construction2008[construction2008$industry=="Insgesamt",]$emp)

# Concentration of financial industry in Germany 2008 vs. 2014
# based on 16 German regions (Bundeslaender) for 2008 and 2014
finance2008 <- G.regions.emp[(G.regions.emp$industry == 
"Erbringung von Finanz- und Vers.leistungen (K)" | 
G.regions.emp$industry == "Insgesamt") & G.regions.emp$year == "2008",]
finance2008 <- finance2008[finance2008$region != "Insgesamt",]
# delete all-over data for all industries
gini.conc(finance2008[finance2008$industry == 
"Erbringung von Finanz- und Vers.leistungen (K)",]$emp, 
finance2008[finance2008$industry=="Insgesamt",]$emp)
finance2014 <- G.regions.emp[(G.regions.emp$industry == 
"Erbringung von Finanz- und Vers.leistungen (K)" | G.regions.emp$industry ==
"Insgesamt") & G.regions.emp$year == "2014",]
finance2014 <- finance2014[finance2014$region != "Insgesamt",]
# delete all-over data for all industries
gini.conc(finance2014[finance2014$industry == 
"Erbringung von Finanz- und Vers.leistungen (K)",]$emp, 
finance2014[finance2014$industry=="Insgesamt",]$emp)
}