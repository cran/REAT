\name{howard.xcl}
\alias{howard.xcl}

\title{
Howard-Newman-Tarp excess colocation (XCL) index
}

\description{
Calculating the excess colocation (XCL) index by Howard, Newman and Tarp for two industries
}

\usage{
howard.xcl(k, industry, region, industry1, industry2, no.samples = 50, e_k = NULL)
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
  \item{industry1}{
Regarded industry 1 (out of the \code{industry} vector)
}
  \item{industry2}{
Regarded industry 2 (out of the \code{industry} vector)
}
  \item{no.samples}{
Number of samples for the counterfactual firm allocation via bootstrapping
}
  \item{e_k}{
Employment of firm \eqn{k}
}
}

\details{
The Howard-Newman-Tarp excess colocation index (\eqn{XCL}) is standardized (\eqn{-1 \le CL \le 1}). The rationale behind is that the CL index (see \code{howard.cl}) is compared to a counterfactual (random) location pattern which is constructed via bootstrapping. Processing time depends on the number of firms and the number of samples.
}

\value{
A single value of \eqn{XCL}
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
# example from Howard et al. (2016):
firms <- 1:6
industries <- c("A", "B", "A", "B", "A", "B")
locations <- c("X", "X", "X", "Y", "Y", "X")

howard.xcl(firms, industries, locations, industry1 = "A", 
industry2 = "B")
}