\name{shiftp}
\alias{shiftp}

\title{
Shift-share prognosis
}

\description{
Forecasting regional employment growth with the shift-share analysis (Gerfin model)
}

\usage{
shiftp(e_ij1, e_ij2, e_i1, e_i2, e_i3, time1, time2, time3,
industry.names = NULL, output.results = TRUE, 
plot.results = FALSE, plot.colours = NULL, plot.title = NULL,
plot.portfolio = FALSE, ...)
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
  \item{e_i3}{
a numeric vector with \eqn{i} values containing the total employment in \eqn{i} industries at time 3 (forecast value for total employment)
}
  \item{time1}{
start year (single value)
}
  \item{time2}{
end year of \emph{empirical} employment data (single value)
}
  \item{time3}{
year of prognosis (single value)
}
  \item{industry.names}{
Industry names (e.g. from the relevant statistical classification of economic activities)
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
The \emph{shift-share analysis} (Dunn 1960) adresses the regional growth (or decline) regarding the over-all development in the national economy. The aim of this analysis model is to identify which parts of the regional economic development can be traced back to national trends, effects of the regional industry structure and (positive) regional factors. The growth (or decline) of regional employment consists of three factors: \eqn{l_{t+1}-l_t = nps + nds + nts}, where \eqn{l} is the employment in the region at time \eqn{t} and \eqn{t+1}, respectively, and \eqn{nps} is the \emph{net proportionality shift}, \eqn{nds} is the \emph{net differential shift} and \eqn{nts} is the \emph{net total shift}. Other variants are e.g. the shift-share method by Gerfin (Index method), the dynamic shift-share analysis (Barff/Knight 1988) or the extension by Esteban-Marquillas (1972). 

As there is more than one way to calculate a Dunn-type \emph{shift-share analysis} and the terms are not used consequently in the regional economic literature, this function and the documentation use the formulae and terms given in Farhauer/Kroell (2013). If \code{shift.method = "Dunn"}, this function calculates the \emph{net proportionality shift} (\eqn{nps}), the \emph{net differential shift} (\eqn{nds}) and the \emph{net total shift} (\eqn{nts}) where the last one represents the residuum of (positive) regional factors.

This function calculates an employment prognosis based on a Gerfin shift-share analysis for two years.
}

\value{
A \code{list} containing the following objects:
\item{components }{A \code{matrix} containing the shift-share components related to the chosen method}
\item{growth }{A \code{matrix} containing the industry-specific growth values}
\item{prog }{A \code{matrix} containing the industry-specific prognosis values}
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

Spiekermann, K./Wegener, M. (2008): \dQuote{Modelle in der Raumplanung I. 4 - Input-Output-Modelle}. Power Point presentation. \url{http://www.spiekermann-wegener.de/mir/pdf/MIR1_4_111108.pdf}.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{portfolio}}, \code{\link{shiftd}}, \code{\link{shifti}}, , \code{\link{shift.growth}}
}

\examples{
# Example data from Spiekermann/Wegener 2008:
# two regions, two industries
region1_2000 <- c(1400, 3600)
region1_2006 <- c(1000, 4400)
region2_2000 <- c(1200, 1800)
region2_2006 <- c(1100, 3700)
region3_2000 <- c(1100, 900)
region3_2006 <- c(800, 1000)
# regional values
nation_2000 <- c(3700, 6300)
nation_2006 <- c(2900, 9100)
# national values
nation_2010 <- c(2500, 12500)
# national prognosis values

# Analysis for region 1:
shiftp(region1_2000, region1_2006, nation_2000,
nation_2006, e_i3 = nation_2010, 
time1 = 2000, time2 = 2006, time3 = 2010)
# Analysis for region 2:
shiftp(region2_2000, region2_2006, nation_2000, 
nation_2006, e_i3 = nation_2010, 
time1 = 2000, time2 = 2006, time3 = 2010)
# Analysis for region 3:
shiftp(region3_2000, region3_2006, nation_2000, 
nation_2006, e_i3 = nation_2010, 
time1 = 2000, time2 = 2006, time3 = 2010)
}