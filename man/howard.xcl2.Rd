\name{howard.xcl2}
\alias{howard.xcl2}

\title{
Howard-Newman-Tarp excess colocation (XCL) index
}

\description{
Calculating the excess colocation (XCL) index by Howard, Newman and Tarp for a given number of industries
}

\usage{
howard.xcl2(k, industry, region, print.results = TRUE)
}

\arguments{
  \item{k}{
a vector containing the IDs/names of firms \eqn{k}
}
  \item{industry}{
a vector containing the IDs/names of the industries \eqn{i}
}
  \item{region}{
a vector containing the IDs/names of the regions \eqn{j}
}
  \item{print.results}{
logical argument that indicates whether the calculated values are printed or not
}
}

\details{
The Howard-Newman-Tarp excess colocation index (\eqn{XCL}) is standardized (\eqn{-1 \le CL \le 1}). The rationale behind is that the CL index (see \code{howard.cl}) is compared to a counterfactual (random) location pattern which is constructed via bootstrapping. Processing time depends on the number of firms and the number of samples. This function takes a while even for a relatively small number of industries!
}

\value{
A matrix with \eqn{I} rows (one for each industry-industry combination) containing the \eqn{XCL} values
}

\references{
Howard, E./Newman, C./Tarp, F. (2016): \dQuote{Measuring industry coagglomeration and identifying the driving forces}. In: \emph{Journal of Economic Geography}, \bold{16}, 5, p. 1055-1078.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{howard.cl}}, \code{\link{howard.xcl2}}, \code{\link{ellison.c}}, \code{\link{ellison.c2}}
}

\examples{
\dontrun{
# example data from Farhauer/Kroell (2014):
data (FK2014_EGK)

howard.xcl2 (FK2014_EGK$Betrieb, FK2014_EGK$Branche, 
FK2014_EGK$Region)
# this may take a while!
}
}