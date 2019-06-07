\name{shift.growth}
\alias{shift.growth}

\title{
Growth rates for shift-share analysis
}

\description{
This function calculates industry-specific growth rates which are part of the shift-share analysis
}

\usage{
shift.growth(e_ij1, e_ij2, e_i1, e_i2, time.periods = NULL, 
industry.names = NULL)
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
  \item{time.periods}{
No. of regarded time periods (for average growth rates)
}
  \item{industry.names}{
Industry names (e.g. from the relevant statistical classification of economic activities)
}
}

\details{
The \emph{shift-share analysis} (Dunn 1960) adresses the regional growth (or decline) regarding the over-all development in the national economy. The aim of this analysis model is to identify which parts of the regional economic development can be traced back to national trends, effects of the regional industry structure and (positive) regional factors. The growth (or decline) of regional employment consists of three factors: \eqn{l_{t+1}-l_t = nps + nds + nts}, where \eqn{l} is the employment in the region at time \eqn{t} and \eqn{t+1}, respectively, and \eqn{nps} is the \emph{net proportionality shift}, \eqn{nds} is the \emph{net differential shift} and \eqn{nts} is the \emph{net total shift}. Other variants are e.g. the shift-share method by Gerfin (Index method) and the dynamic shift-share analysis (Barff/Knight 1988).

As there is more than one way to calculate a Dunn-type \emph{shift-share analysis} and the terms are not used consequently in the regional economic literature, this function and the documentation use the formulae and terms given in Farhauer/Kroell (2013). If \code{shift.method = "Dunn"}, this function calculates the \emph{net proportionality shift} (\eqn{nps}), the \emph{net differential shift} (\eqn{nds}) and the \emph{net total shift} (\eqn{nts}) where the last one represents the residuum of (positive) regional factors.

This function calculates industry-specific growth rates which are part of a shift-share analysis.
}

\value{
A \code{matrix} containing the industry-specific growth values
}

\references{
Arcelus, F. J. (1984): \dQuote{An Extension of Shift-Share Analysis}. In: In: \emph{Growth and Change}, \bold{15}, 1, p. 3-8.

Barff, R. A./Knight, P. L. (1988): \dQuote{Dynamic Shift-Share Analysis}. In: \emph{Growth and Change}, \bold{19}, 2, p. 1-10.

Casler, S. D. (1989): \dQuote{A Theoretical Context for Shift and Share Analysis}. In: \emph{ Regional Studies}, \bold{23}, 1, p. 43-48.

Dunn, E. S. Jr. (1960): \dQuote{A statistical and analytical technique for regional analysis}. In: \emph{Papers and Proceedings of the Regional Science Association}, \bold{6}, p. 97-112.

Esteban-Marquillas, J. M. (1972): \dQuote{Shift- and share analysis revisited}. In: \emph{Regional and Urban Economics}, \bold{2}, 3, p. 249-261.

Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.

Gerfin, H. (1964): \dQuote{Gesamtwirtschaftliches Wachstum und regionale Entwicklung}. In: \emph{Kyklos}, \bold{17}, 4, p. 565-593.

Goschin, Z. (2014): \dQuote{Regional growth in Romania after its accession to EU: a shift-share analysis approach}. In: \emph{Procedia Economics and Finance}, \bold{15}, p. 169-175.

Schoenebeck, C. (1996): \dQuote{Wirtschaftsstruktur und Regionalentwicklung: Theoretische und empirische
Befunde fuer die Bundesrepublik Deutschland}. \emph{Dortmunder Beitraege zur Raumplanung}, \bold{75}. Dortmund.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{portfolio}}, \code{\link{shift}}, \code{\link{shiftd}}, \code{\link{shifti}}
}

\examples{
# Example from Farhauer/Kroell (2013):
region_A_t <- c(90,20,10,60)
region_A_t1 <- c(100,40,10,55)
# data for region A (time t and t+1)
nation_X_t <- c(400,150,150,400)
nation_X_t1 <- c(440,210,135,480)
# data for the national economy (time t and t+1)
shift.growth(region_A_t, region_A_t1, nation_X_t, nation_X_t1)
}