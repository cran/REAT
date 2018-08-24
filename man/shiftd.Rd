\name{shiftd}
\alias{shiftd}

\title{
Dynamic shift-share analysis
}

\description{
Analyzing regional growth with the dynamic shift-share analysis 
}

\usage{
shiftd(region1, region2, nation1, nation2, time1, time2, 
industry.names = NULL, shift.method = "Dunn", 
gerfin.shifts = "sum", output.results = TRUE, 
plot.results = FALSE, plot.colours = NULL, plot.title = NULL, 
plot.portfolio = FALSE, ...)
}

\arguments{
  \item{region1}{
a numeric vector with \eqn{i} values containing the employment in \eqn{i} industries in a region at time 1
}
  \item{region2}{
a data frame with \eqn{i} rows containing the employment in \eqn{i} industries in a region for \eqn{j} years
}
  \item{nation1}{
a numeric vector with \eqn{i} values containing the employment in \eqn{i} industries in the whole nation at time 1
}
  \item{nation2}{
a data frame with \eqn{i} rows containing the employment in \eqn{i} industries in the whole nation for \eqn{j} years
}
  \item{time1}{
Initial year
}
  \item{time2}{
Final year
}
  \item{industry.names}{
Industry names
}
  \item{shift.method}{
Method of shift-share-analysis to be used ("Dunn", "Gerfin") (default: \code{shift.method = "Dunn"})
}
  \item{gerfin.shifts}{
If \code{shift.method = "Gerfin"}: Logical argument that indicates if the shifts are calculated as sums or as means (default: \code{gerfin = "sum"})
}
  \item{output.results}{
Logical argument that indicates if the function shows the results or not
}
  \item{plot.results}{
Logical argument that indicates if the results have to be plotted
}
  \item{plot.colours}{
If \code{plot.results = TRUE}: Plot colours
}
  \item{plot.title}{
If \code{plot.results = TRUE}: Plot title
}
  \item{plot.portfolio}{
Logical argument that indicates if the results have to be plotted in a portfolio matrix additionally
}
  \item{\dots}{
Additional arguments for the portfolio plot (see the function \code{portfolio})
}
}

\details{
The \emph{shift-share analysis} (Dunn 1960) adresses the regional growth (or decline) regarding the over-all development in the national economy. The aim of this analysis model is to identify which parts of the regional economic development can be traced back to national trends, effects of the regional industry structure and (positive) regional factors. The growth (or decline) of regional employment consists of three factors: \eqn{l_{t+1}-l_t = nps + nds + nts}, where \eqn{l} is the employment in the region at time \eqn{t} and \eqn{t+1}, respectively, and \eqn{nps} is the \emph{net proportionality shift}, \eqn{nds} is the \emph{net differential shift} and \eqn{nts} is the \emph{net total shift}. Other variants are e.g. the shift-share method by Gerfin (Index method) and the dynamic shift-share analysis (Barff/Knight 1988).

As there is more than one way to calculate a Dunn-type \emph{shift-share analysis} and the terms are not used consequently in the regional economic literature, this function and the documentation use the formulae and terms given in Farhauer/Kroell (2013). If \code{shift.method = "Dunn"}, this function calculates the \emph{net proportionality shift} (\eqn{nps}), the \emph{net differential shift} (\eqn{nds}) and the \emph{net total shift} (\eqn{nts}) where the last one represents the residuum of (positive) regional factors.

This function calculates a dynamic shift-share analysis for at least two years.
}

\value{
A \code{list} containing the following objects:
\item{components }{A \code{matrix} containing the shift-share components related to the chosen method}
\item{components.year }{A \code{matrix} containing the shift-share components for each year}
\item{growth }{A \code{matrix} containing the industry-specific growth values}
\item{method }{The chosen method, e.g. "Dunn"}
}

\references{
Arcelus, F. J. (1984): \dQuote{An Extension of Shift-Share Analysis}. In: In: \emph{Growth and Change}, \bold{15}, 1, p. 3-8.

Barff, R. A./Knight, P. L. (1988): \dQuote{Dynamic Shift-Share Analysis}. In: \emph{Growth and Change}, \bold{19}, 2, p. 1-10.

Casler, S. D. (1989): \dQuote{A Theoretical Context for Shift and Share Analysis}. In: \emph{ Regional Studies}, \bold{23}, 1, p. 43-48.

Dunn, E. S. Jr. (1960): \dQuote{A statistical and analytical technique for regional analysis}. In: \emph{Papers and Proceedings of the Regional Science Association}, \bold{6}, p. 97-112.

Esteban-Marquillas, J. M. (1972): \dQuote{Shift- and share analysis revisited}. In: \emph{Regional and Urban Economics}, \bold{2}, 3, p. 249-261.

Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Gerfin, H. (1964): \dQuote{Gesamtwirtschaftliches Wachstum und regionale Entwicklung}. In: \emph{Kyklos}, \bold{17}, 4, p. 565-593.

Schoenebeck, C. (1996): \dQuote{Wirtschaftsstruktur und Regionalentwicklung: Theoretische und empirische
Befunde fuer die Bundesrepublik Deutschland}. \emph{Dortmunder Beitraege zur Raumplanung}, \bold{75}. Dortmund.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{portfolio}}, \code{\link{shift}}, \code{\link{shifti}}, \code{\link{shift.growth}}
}

\examples{
# Example from Farhauer/Kroell (2013), extended:
region_A_t <- c(90,20,10,60)
region_A_t1 <- c(100,40,10,55)
region_A_t2 <- c(105,45,15,60)
# data for region A (time t and t+1)
nation_X_t <- c(400,150,150,400)
nation_X_t1 <- c(440,210,135,480)
nation_X_t2 <- c(460,230,155,500)
# data for the national economy (time t and t+1)
shiftd(region_A_t, data.frame(region_A_t1, region_A_t2), nation_X_t, 
data.frame(nation_X_t1, nation_X_t2), time1 = 2000, time2 = 2002,
plot.results = TRUE, plot.portfolio = TRUE, psize = region_A_t1)
}