\name{portfolio}
\alias{portfolio}

\title{
Portfolio matrix
}
\description{
Portfolio matrix plot comparing two numeric vectors
}
\usage{
portfolio(x, y, z, label_x = "X", label_y = "Y", heading = "Portfolio", 
pcol = "given", colsp = 0, leg = FALSE, leg_vec = 0, leg_fsize = 1, 
leg_x = -max_val, leg_y = -max_val/2)
}

\arguments{
  \item{x}{
A numeric vector representing the values for the x axis
}
  \item{y}{
A numeric vector representing the values for the y axis
}
  \item{z}{
A numeric vector representing the size of the points/bubbles
}
  \item{label_x}{
Label for the x axis
}
  \item{label_y}{
Label for the y axis
}
  \item{heading}{
Heading for the plot
}
  \item{pcol}{
indicates if the colors of the points are given by the user (\code{pcol = "given"}) and defined by the vector \code{colsp} or set by random (\code{pcol = "random"})  
}
  \item{colsp}{
a vector representing the user-defined colors of the points
}
  \item{leg}{
logical argument that indicates if the plot has a legend or not (default: \code{leg = FALSE})
}
  \item{leg_vec}{
if \code{leg = TRUE}, this vector defines the values for the plot legend
}
  \item{leg_fsize}{
if \code{leg = TRUE}, this value defines the font size of the legend
}
  \item{leg_x}{
if \code{leg = TRUE}: x coordinate for the legend (default: \code{leg_x=-max_val}, where \code{max_val} is the maximum value of all values in the dataset)
}
  \item{leg_y}{
if \code{leg = TRUE}: y coordinate for the legend (default: \code{leg_y=-max_val/2}, where \code{max_val} is the maximum value of all values in the dataset)
}
}
\details{
The \emph{portfolio matrix} is a graphic tool displaying the development of one variable compared to another variable. The plot shows the regarded variable on the \eqn{x} axis and a variable with which it is confronted on the \eqn{y} axis while the graph is divided in four quadrants. Originally, the \emph{portfolio matrix} was developed by the \emph{Boston Consulting Group} to analyze the performance of product lines in marketing, also known as the \emph{growth-share matrix}. The quadrants show the performace of the regarded objects (stars, cash cows, question marks, dogs) (Henderson 1973). But the \emph{portfolio matrix} can also be used to analyze/illustrate the world market integration of a region or a national economy by confronting e.g. the increase in world market share (\eqn{x} axis) and the world trade growth (\eqn{y} axis) (Baker et al. 2002). Another option is to analyze/illustrate the economic performance of a region (Howard 2007). E.g. it is possible to confront the growth of industries in a region with the all-over growth of these industries in the national economy. 
}
\value{
A plot of the portfolio matrix
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
\code{\link{shift}}
}
\examples{
data(Freiburg)
# Loads the data
industries <- Freiburg$industry
x <- Freiburg$e_g_Freiburg_0814
y <- Freiburg$e_g_Germany_0814
z <- Freiburg$e_Freiburg2014
portfolio(x,y,z, "Freiburg", "Germany", "Growth portfolio Freiburg and Germany",
pcol="given", colsp=Freiburg$color, leg=1, leg_vec=industries, leg_fsize=0.6)
# Creates a portfolio comparing the industry growth in Freiburg and Germany
}