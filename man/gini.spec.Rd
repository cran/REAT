\name{gini.spec}
\alias{gini.spec}

\title{
Gini coefficient of regional specialization
}
\description{
Calculating the Gini coefficient of regional specialization based on regional industry data (normally employment data)
}
\usage{
gini.spec(e_ij, e_i, lc = FALSE, lcx = "\% of objects", 
lcy = "\% of regarded variable", lctitle = "Lorenz curve", 
le.col = "blue", lc.col = "black", lsize = 1, ltype = "solid",
bg.col = "gray95", bgrid = TRUE, bgrid.col = "white", 
bgrid.size = 2, bgrid.type = "solid", lcg = FALSE, lcgn = FALSE, 
lcg.caption = NULL, lcg.lab.x = 0, lcg.lab.y = 1, 
add.lc = FALSE, plot.lc = TRUE)
}

\arguments{
  \item{e_ij}{
a numeric vector with the employment of the industries \eqn{i} in region \eqn{j}
}
  \item{e_i}{
a numeric vector with the employment in the industries \eqn{i}
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
The \emph{Gini coefficient of regional specialization} (\eqn{G_{j}}) is a special spatial modification of the \emph{Gini coefficient} of inequality (see the function \code{gini()}). It represents the degree of regional specialization of the region \eqn{j} referring to \eqn{i} industries.  The coefficient \eqn{G_{j}} varies between 0 (no specialization) and 1 (complete specialization). Optionally a Lorenz curve is plotted (if \code{lc = TRUE}).
}

\value{
A single numeric value (\eqn{0 < G_{j} < 1})
}

\references{
Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Nakamura, R./Morrison Paul, C. J. (2009): \dQuote{Measuring agglomeration}. In: Capello, R./Nijkamp, P. (eds.): \emph{Handbook of Regional Growth and Development Theories}. Cheltenham: Elgar. p. 305-328.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{gini.conc}}
}

\examples{
# Example from Farhauer/Kroell (2013):
E_ij <- c(700,600,500,10000,40000)
# employment of five industries in the region
E_i <- c(30000,15000,10000,60000,50000)
# over-all employment in the five industries
gini.spec (E_ij, E_i)
# Returns the Gini coefficient of regional specialization (0.6222222)

# Example Freiburg
data(Freiburg)
# Loads the data
E_ij <- Freiburg$e_Freiburg2014
# industry-specific employment in Freiburg 2014
E_i <- Freiburg$e_Germany2014
# industry-specific employment in Germany 2014
gini.spec (E_ij, E_i)
# Returns the Gini coefficient of regional specialization (0.2089009)

# Example Goettingen
data(Goettingen)
# Loads the data
gini.spec(Goettingen$Goettingen2017[2:16], Goettingen$BRD2017[2:16])
# Returns the Gini coefficient of regional specialization 2017 (0.359852)
}