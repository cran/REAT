\name{gifpro.tbs}
\alias{gifpro.tbs}

\title{
Trend-based and location-specific commercial area prognosis
}

\description{
This function contains the TBS-GIFPRO model for commercial area prognosis (TBS-GIFPRO = Trendbasierte und standortspezifische Gewerbe- und Industrieflaechenprognose; trend-based and location-specific commercial area prognosis)
}

\usage{
gifpro.tbs(e_ij, a_i, sq_ij, rq_ij, ru_ij = NULL, ai_ij, 
time.base, tinterval = 1, prog.func = rep("lin", nrow(e_ij)), 
prog.plot = TRUE, plot.single = FALSE,
industry.names = NULL, emp.only = FALSE, output = "short")
}

\arguments{
  \item{e_ij}{
a numeric vector with \eqn{i} values containing the current employment in \eqn{i} industries in region \eqn{j}
}
  \item{a_i}{
a numeric vector with \eqn{i} values containing the share of employees in industry \eqn{i} which is located in commercial areas
}
  \item{sq_ij}{
a numeric vector with \eqn{i} values containing the annual quote of resettled employees (\emph{Neuansiedlungsquote} in German) in industry \eqn{i}, in percent
}
  \item{rq_ij}{
a numeric vector with \eqn{i} values containing the annual quote of relocated employees (\emph{Verlagerungsquote} in German) in industry \eqn{i}, in percent
}
  \item{ru_ij}{
a numeric vector with \eqn{i} values containing the annual quote of employees in industry \eqn{i} which is located in reused commercial area (\emph{Wiedernutzungsquote} in German), in percent (default: \code{ru_ij = NULL}, which represents a quote of 0 percent, meaning that no commercial area can be reused)
}
  \item{ai_ij}{
a numeric vector with \eqn{i} values containing the areal index (\emph{Flaechenkennziffer} in German), representing the area requirement (e.g. in sqm) per employee in industry \eqn{i}
}
  \item{time.base}{
a single value representing the start time of the prognose (typically current year + 1)
}
  \item{tinterval}{
a single value representing the forecast horizon (length of time into the future for which the commercial area prognosis is done), in time units (e.g. \code{tinterval = 10} = 10 years)
}
  \item{prog.func}{
a vector containing the estimation function types for employment prognosis ("lin" for linear, "pow" for power, "exp" for exponential and "logi" for logistic function); must have the same length as \code{e_ij} and \code{industry.names}, respectively
}
  \item{prog.plot}{
Logical argument that indicates if the employment prognoses have to be plotted
}
  \item{plot.single}{
If \code{prog.plot = TRUE}: Logical argument that indicates if the plots are stored as single graphic devices or integrated in one plot
}
  \item{industry.names}{
a vector containing the industry names (e.g. from the relevant statistical classification of economic activities)
}
  \item{emp.only}{
Logical argument that indicates if the analysis only contains employment prognosis
}
  \item{output}{
Type of output: \code{output = "short"} (default) shows the final number of relevant employment and commercial area requirement. If \code{output = "full"}, employment and commercial area are displayed for each time unit (year)
}
}

\details{
In municipal land use planning (mostly in Germany), the future need of local commercial area (which is a type of land use, defined in official land-use plans) is mostly forecasted by models founded on the GIFPRO model (\emph{\bold{G}ewerbe- und \bold{I}ndustrie\bold{f}laechenbedarfs\bold{pro}gnose}, prognosis of future demand of commercial area). GIFPRO is a demand-side model, which means predicting the demand of commercial area based on a prognosis of future employment in different industries (Bonny/Kahnert 2005). The key parameters of the model are the (assumed) shares of employees located in commercial areas (\eqn{a_i}), the (assumed) quotas of resettlement (\eqn{sq_{ij}}), relocation (\eqn{rq_{ij}}) and (sometimes) reuse (\eqn{ru_{ij}}) as well as the (assumed) area requirement per employee (\eqn{ai_{ij}}). Outgoing from current employment in \eqn{i} industries in region \eqn{j}, \eqn{e_{ij}}, the future employment is predicted based on the quotas mentioned above and, finally, multiplied by the industry-specific (and maybe region-specific) areal index. The GIFPRO model has been modified and extended several times, especially with respect to industry- and region-specific employment growth, quotas and areal indices (Deutsches Institut fuer Urbanistik 2010, Vallee et al. 2012).

This function contains the TBS-GIFPRO model for commercial area prognosis (TBS-GIFPRO = Trendbasierte und standortspezifische Gewerbe- und Industrieflaechenprognose; trend-based and location-specific commercial area prognosis) (Deutsches Institut fuer Urbanistik 2010).
}

\value{
A \code{list} containing the following objects:
\item{components }{List with matrices containing the single components (resettlement, relocation, reuse, relevant employment)}
\item{results }{List with matrices containing the final results per year and all over as well as the industry-specific forecast data}
}

\references{
Bonny, H.-W./Kahnert, R. (2005): \dQuote{Zur Ermittlung des Gewerbeflaechenbedarfs: Ein Vergleich zwischen einer Monitoring gestuetzten Prognose und einer analytischen Bestimmung}. In: \emph{Raumforschung und Raumordnung}, \bold{63}, 3, p. 232-240.

Deutsches Institut fuer Urbanistik (ed.) (2010): \dQuote{Stadtentwicklungskonzept Gewerbe fuer die Landeshauptstadt Potsdam}. Berlin. \url{https://www.potsdam.de/sites/default/files/documents/STEK_Gewerbe_Langfassung_2010.pdf} (accessed October 13, 2017).

Vallee, D./Witte, A./Brandt, T./Bischof, T. (2012): \dQuote{Bedarfsberechnung fuer die Darstellung von Allgemeinen Siedlungsbereichen (ASB) und Gewerbe- und Industrieansiedlungsbereichen (GIB) in Regionalplaenen}. Im Auftrag der Staatskanzlei des Landes Nordrhein-Westfalen. Abschlussbericht Oktober 2012. Aachen. \url{https://www.land.nrw/sites/default/files/asset/document/lep_nrw_flaechenbedarf_endbericht_endfassung_04122012.pdf} (accessed August 16, 2018).
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gifpro}}, \code{\link{portfolio}}, \code{\link{shift}}, \code{\link{shiftd}}, \code{\link{shifti}}
}

\examples{
# Data for Goettingen:
data(Goettingen)

anteileGOE <- rep(100,15)
nvquote <- rep (0.3, 15)
vlquote <- rep (0.7, 15)

gifpro.tbs (e_ij = Goettingen[2:16,2:11], a_i = anteileGOE, 
sq_ij = nvquote, rq_ij = vlquote, tinterval = 12, 
prog.func = rep("lin", nrow(Goettingen[2:16,2:11])),
ai_ij = 150, time.base = 2008, output = "full",
industry.names = Goettingen$WA_WZ2008[2:16],
prog.plot = TRUE, plot.single = FALSE)

gifpro.tbs (e_ij = Goettingen[2:16,2:11], 
a_i = anteileGOE, sq_ij = nvquote,
rq_ij = vlquote, tinterval = 12, 
prog.func = rep("lin", nrow(Goettingen[2:16,2:11])),
ai_ij = 150, time.base = 2008, output = "full",
industry.names = Goettingen$WA_WZ2008[2:16],
prog.plot = TRUE, plot.single = TRUE)

gifpro.tbs (e_ij = Goettingen[2:10,2:11], 
a_i = rep(100, 9), sq_ij = rep(100, 9),
rq_ij = rep(100, 9), tinterval = 12, 
prog.func = rep("exp", nrow(Goettingen[2:10,2:11])),
ai_ij = 150, time.base = 2008, output = "full",
industry.names = Goettingen$WA_WZ2008[2:10],
prog.plot = TRUE, plot.single = FALSE)
}