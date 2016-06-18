\name{gini}
\alias{gini}

\title{
Gini coefficient and Lorenz curve
}
\description{
Calculating the Gini coefficient of inequality (or concentration), standardized and non-standardized, and plotting the Lorenz curve
}
\usage{
gini(x, norm = FALSE, lc = FALSE, lcx = "\% of objects", 
lcy = "\% of regarded variable", lctitle = "Lorenz curve", lcg = FALSE, 
lcgn = FALSE)
}

\arguments{
  \item{x}{
A numeric vector (e.g. dataset of household income, sales turnover or supply)
}
  \item{norm}{
logical argument that indicates if the function output is the non-standardized or the standardized Gini coefficient (default: \code{norm = FALSE}, that means the non-standardized Gini coefficient is returned)
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
  \item{lcg}{
if \code{lc = TRUE} (plot of Lorenz curve), the logical argument \code{lcg} defines if the non-standardized Gini coefficient is displayed in the Lorenz curve plot
}
  \item{lcgn}{
if \code{lc = TRUE} (plot of Lorenz curve), the logical argument \code{lcgn} defines if the standardized Gini coefficient is displayed in the Lorenz curve plot
}
}
\details{
The \emph{Gini coefficient} (Gini 1912) is a popular measure of statistical dispersion, especially used for analyzing inequality or concentration of income, wealth or sales turnover of competing firms. The coefficient (\eqn{G}) varies between 0 (no inequality/concentration) and 1 (complete inequality/concentration). The \emph{Lorenz curve} (Lorenz 1905), though developed independently, can be regarded as a graphical representation of the degree of inequality calculated by the \emph{Gini coefficient} and can also be used for additional interpretations of \eqn{G}. The curve displays the deviations of the empirical distribution from a perfectly equal distribution as the difference between two graphs (the distribution curve and a diagonal line of perfect equality). This function calculates \eqn{G} and plots the \emph{Lorenz curve} optionally. As there are several ways to calculate the \emph{Gini coefficient}, this function uses the formula given in Doersam (2004). Because the maximum of \eqn{G} is not equal to 1, also a standardized coefficient (\eqn{G*}) with a maximum equal to 1 can be calculated alternatively.      
}
\value{
A single numeric value of the \emph{Gini coefficient} (\eqn{0 < G < 1}) or the \emph{standardized Gini coefficient} (\eqn{0 < G* < 1}) and, optionally, a plot of the \emph{Lorenz curve}.
}

\references{
Cerlani, L./Verme, P. (2012): \dQuote{The origins of the Gini index: extracts from Variabilita e Mutabilita (1912) by Corrado Gini}. In: \emph{The Journal of Economic Inequality}, \bold{10}, 3, p. 421-443. 

Doersam, P. (2004): \dQuote{Wirtschaftsstatistik anschaulich dargestellt}. Heidenau : PD-Verlag.

Gini, C. (1912): \dQuote{Variabilita e Mutabilita}. Contributo allo Studio delle Distribuzioni e delle Relazioni
Statistiche. Bologna : Cuppini.

Lessmann, C. (2005): \dQuote{Regionale Disparitaeten in Deutschland und ausgesuchten OECD-Staaten im Vergleich}. \emph{ifo Dresden berichtet}, \bold{3/2005}. \url{https://www.cesifo-group.de/link/ifodb_2005_3_25-33.pdf}.

Lorenz, M. O. (1905): \dQuote{Methods of Measuring the Concentration of Wealth}. In: \emph{Publications of the American Statistical Association}, \bold{9}, 70, p. 209-219.

}
\author{
Thomas Wieland
}

\seealso{
\code{\link{cv}}, \code{\link{gini.conc}}, \code{\link{gini.spec}}, \code{\link{herf}}
}
\examples{
# Example from Doersam (2004):
sales <- c(20,50,20,10)
# sales turnover of four car manufacturing companies
gini(sales, lc=TRUE, lcx="percentage of companies", lcy="percentrage of sales", 
lctitle="Lorenz curve of sales", lcg=TRUE, lcgn=TRUE)
# returs the non-standardized Gini coefficient (0.3) and 
# plots the Lorenz curve with user-defined title and labels 
gini(sales, norm=TRUE)
# returns the standardized Gini coefficient (0.4)
}
