\name{mssd}
\alias{mssd}

\title{
Mean square successive difference
}

\description{
Calculating the mean square successive difference
}
\usage{
mssd (x)
}

\arguments{
  \item{x}{
a \code{numeric} vector arranged in chronological order
}
}

\details{
The \emph{mean square successive difference}, \eqn{\delta^2}, is a dimensionless measure of variability over time (von Neumann et al. 1941). It can be used for assessing the volatility of a variable with respect to different subjects/groups.
}

\value{
Single numeric value (the \emph{mean square successive difference}, \eqn{\delta^2}). 
}

\references{
Von Neumann, J./Kent, R. H./Bellinson, H. R./Hart, B. I. (1941): \dQuote{The mean square successive difference}. In: \emph{The Annals of Mathematical Statistics}, \bold{12}, 2, p. 153-162. 
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{var2}}, \code{\link{sd2}}, \code{\link{cv}}
}

\examples{
data1 <- c(10,10,10,20,20,20,30,30,30)
# stable growth
data2 <- c(20,10,30,10,30,20,30,20,10)
# high variability

# Means:
mean2(data1)
mean2(data2)
# Same means

# Standard deviation:
sd2(data1)
sd2(data2)
# Coefficient of variation:
cv(data1)
cv(data2)
# Measures of statistical dispersion are equal

mssd(data1)
mssd(data2)
# high differences in variability
}