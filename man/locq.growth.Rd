\name{locq.growth}
\alias{locq.growth}

\title{
Portfolio matrix for specialization and growth
}

\description{
Portfolio matrix plot comparing two numeric vectors (here: specialization and growth)
}

\usage{
locq.growth(e_ij1, e_ij2, e_i1, e_i2, industry.names = NULL, 
y.axis = "r", 
psize, psize.factor = 10, 
pmx = "Regional specialization", pmy = "Regional growth", 
pmtitle = "Portfolio matrix", pcol = NULL, pcol.border = NULL,
leg = FALSE, leg.fsize = 1, 
leg.x = 0, leg.y = y_min*1.5,
bg.col = "gray95", bgrid = TRUE, bgrid.col = "white", 
bgrid.size = 2, bgrid.type = "solid",
seg.x = 1, seg.y = 0)
}

\arguments{
  \item{e_ij1}{
a numeric vector with \eqn{i} values containing the employment in \eqn{i} industries in region \eqn{j} at time 1
}
  \item{e_ij2}{
a numeric vector with \eqn{i} values containing the employment in \eqn{i} industries in region \eqn{j} at time 2
}
  \item{e_i1}{
a numeric vector with \eqn{i} values containing the total employment in \eqn{i} industries at time 1
}
  \item{e_i2}{
a numeric vector with \eqn{i} values containing the total employment in \eqn{i} industries at time 2
}
  \item{industry.names}{
Industry names (e.g. from the relevant statistical classification of economic activities)
}
  \item{y.axis}{
Declares which values shall be plotted on the Y axis: If \code{y.axis = "r"}, the Y axis shows the \emph{regional} growth. If \code{y.axis = "n"}, the Y axis shows the \emph{national} growth. To set both growths in ratio, choose \code{y.axis = "rn"} (regional vs. national growth)
}
  \item{psize}{
Point size in the portfolio matrix plot (mostly the absolute values of employment in \eqn{i} industries in region \eqn{j} at time 2)
}
  \item{psize.factor}{
Enlargement factor for the points in the plot
}
  \item{pmx}{
Name of the X axis in the plot
}
  \item{pmy}{
Name of the Y axis in the plot
}
  \item{pmtitle}{
Plot title
}
  \item{pcol}{
Industry-specific point colors
}
  \item{pcol.border}{
Color of point border
}
  \item{leg}{
Logical argument that indicates if a legend has to be added to the plot
}
  \item{leg.fsize}{
If \code{leg = TRUE}: Font size in the plot legend
}
  \item{leg.x}{
If \code{leg = TRUE}: X coordinate of the legend
}
  \item{leg.y}{
If \code{leg = TRUE}: Y coordinate of the legend
}
  \item{bg.col}{
Background color
}
  \item{bgrid}{
Logical argument that indicates if a grid has to be added to the plot
}
  \item{bgrid.col}{
If \code{bgrid = TRUE}: Color of the grid
}
  \item{bgrid.size}{
If \code{bgrid = TRUE}: Size of the grid
}
  \item{bgrid.type}{
If \code{bgrid = TRUE}: Type of the grid
}
  \item{seg.x}{
X coordinate of segmentation of the plot
}
  \item{seg.y}{
Y coordinate of segmentation of the plot
}
}

\details{
The \emph{portfolio matrix} is a graphic tool displaying the development of one variable compared to another variable. The plot shows the regarded variable on the \eqn{x} axis and a variable with which it is confronted on the \eqn{y} axis while the graph is divided in four quadrants. Originally, the \emph{portfolio matrix} was developed by the \emph{Boston Consulting Group} to analyze the performance of product lines in marketing, also known as the \emph{growth-share matrix}. The quadrants show the performace of the regarded objects (stars, cash cows, question marks, dogs) (Henderson 1973). But the \emph{portfolio matrix} can also be used to analyze/illustrate the world market integration of a region or a national economy by confronting e.g. the increase in world market share (\eqn{x} axis) and the world trade growth (\eqn{y} axis) (Baker et al. 2002). Another option is to analyze/illustrate the economic performance of a region (Howard 2007). E.g. it is possible to confront the growth of industries in a region with the all-over growth of these industries in the national economy. 

This function is a special case of portfolio matrix, showing the regional specialization on the X axis instead of the regional growth (which can be plotted on the Y axis).
}

\value{
A portfolio matrix plot.

Invisible: a \code{list} containing the following items:
\item{portfolio.data }{The data related to the plot}
\item{locq }{The localization quotients for each year}
\item{growth }{The growth values for each industry}
}

\references{
Baker, P./von Kirchbach, F./Mimouni, M./Pasteels, J.-M. (2002): \dQuote{Analytical tools for enhancing the participation of developing countries in the Multilateral Trading System in the context of the Doha Development Agenda}. In: \emph{Aussenwirtschaft}, \bold{57}, 3, p. 343-372.

Howard, D. (2007): \dQuote{A regional economic performance matrix - an aid to regional economic policy development}. In: \emph{Journal of Economic and Social Policy}, \bold{11}, 2, Art. 4.

Henderson, B. D. (1973): \dQuote{The Experience Curve - Reviewed, IV. The Growth Share Matrix or The Product Portfolio}. The Boston Consulting Group (BCG).
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{locq}}, \code{\link{portfolio}}, \code{\link{shift}}, \code{\link{shiftd}}, \code{\link{shifti}}
}

\examples{
data(Goettingen)
# Loads employment data for Goettingen and Germany (2008-2017)

locq.growth(Goettingen$Goettingen2008[2:16], Goettingen$Goettingen2017[2:16],
Goettingen$BRD2008[2:16], Goettingen$BRD2017[2:16], 
psize = Goettingen$Goettingen2017[2:16], 
industry.names = Goettingen$WA_WZ2008[2:16], pcol.border = "grey", 
leg = TRUE, leg.fsize = 0.4, leg.x = -0.2)
}