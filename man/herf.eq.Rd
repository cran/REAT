\name{herf.eq}
\alias{herf.eq}

\title{
Herfindahl-Hirschman coefficient equivalent number
}
\description{
Calculating the Herfindahl-Hirschman coefficient equivalent number of objects
}
\usage{
herf.eq(x)
}

\arguments{
  \item{x}{
A numeric vector (e.g. dataset of sales turnover or size of firms)
}
}

\details{
The \emph{equivalent number} referring to the \emph{Herfindahl-Hirschman coefficient} (see the function \code{herf()}) reflects the theoretical number of economic objects (normally firms) where a calculated coefficient is \eqn{\frac{1}{n}} (parity).
}
\value{
A single numeric value of the \emph{Herfindahl-Hirschman coefficient equivalent number}.
}
\references{
Doersam, P. (2004): \dQuote{Wirtschaftsstatistik anschaulich dargestellt}. Heidenau : PD-Verlag.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{cv}}, \code{\link{gini}}, \code{\link{herf}}
}

\examples{
# Example from Doersam (2004):
sales <- c(20,50,20,10)
# sales turnover of four car manufacturing companies
herf(sales)
# returns the HHI (0.34)
herf.eq(sales)
# returns the HHI equivalent number (2.941176)

# Regional disparities in Germany:
gdp <- c(460.69, 549.19, 124.16, 65.29, 31.59, 109.27, 263.44, 39.87, 258.53, 
645.59, 131.95, 35.03, 112.66, 56.22, 85.61, 56.81)
# GDP of german regions 2015 (in billion EUR)
herf(gdp)
# returns the HHI (0.125)
herf.eq(gdp)
# returns the HHI equivalent number (7.988733)
}