\name{shift}
\alias{shift}

\title{
Shift-share analysis
}
\description{
Analyzing regional growth with the shift-share analysis 
}
\usage{
shift(region_t, region_t1, nation_t, nation_t1)
}

\arguments{
  \item{region_t}{
a numeric vector with \eqn{i} values containing the employment in \eqn{i} industries in a region at time \eqn{t} 
}
  \item{region_t1}{
a numeric vector with \eqn{i} values containing the employment in \eqn{i} industries in a region at time \eqn{t+1}
}
  \item{nation_t}{
a numeric vector with \eqn{i} values containing the employment in \eqn{i} industries in the national economy at time \eqn{t}
}
  \item{nation_t1}{
a numeric vector with \eqn{i} values containing the employment in \eqn{i} industries in the national economy at time \eqn{t+1}
}
}
\details{
The \emph{shift-share analysis} (Dunn 1960) adresses the regional growth (or decline) regarding the over-all development in the national economy. The aim of this analysis model is to identify which parts of the regional economic development can be traced back to national trends, effects of the regional industry structure and (positive) regional factors. As there is more than one way to calculate a \emph{shift-share analysis} and the terms are not used consequently in the regional economic literature, this function and the documentation use the formulas and terms given in Farhauer/Kroell (2013). This function calculates the \emph{net proportionality shift} (\eqn{nps}), the \emph{net differential shift} (\eqn{nds}) and the \emph{net total shift} (\eqn{nts}) where the last one represents the residuum of (positive) regional factors.   
}
\value{
a list with three values (\eqn{nps, nds, nts})
}
\references{
Casler, S. D. (1989): \dQuote{A Theoretical Context for Shift and Share Analysis}. In: \emph{ Regional Studies}, \bold{23}, 1, p. 43-48.

Dunn, E. S. Jr. (1960): \dQuote{A statistical and analytical technique for regional analysis}. In: \emph{Papers and Proceedings of the Regional Science Association}, \bold{6}, p. 97-112.

Farhauer, O./Kroell, A. (2013): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{portfolio}}
}

\examples{
# Example from Farhauer/Kroell (2013):
region_A_t <- c(90,20,10,60)
region_A_t1 <- c(100,40,10,55)
# data for region A (time t and t+1)
nation_X_t <- c(400,150,150,400)
nation_X_t1 <- c(440,210,135,480)
# data for the national economy (time t and t+1)
resultsA <- shift(region_A_t, region_A_t1, nation_X_t, nation_X_t1)
# results for region A
region_B_t <- c(60,30,30,40)
region_B_t1 <- c(85,55,40,35)
# data for region B (time t and t+1)
resultsB <- shift(region_B_t, region_B_t1, nation_X_t, nation_X_t1)
# results for region B
region_C_t <- c(250,100,110,300)
region_C_t1 <- c(255,115,85,390)
# data for region C (time t and t+1)
resultsC <- shift(region_C_t, region_C_t1, nation_X_t, nation_X_t1)
# results for region C

# Example Freiburg dataset
data(Freiburg)
# Loads the data
shift(Freiburg$e_Freiburg2008, Freiburg$e_Freiburg2014, Freiburg$e_Germany2008, 
Freiburg$e_Germany2014)
# results for Freiburg and Germany (2008 vs. 2014)
}