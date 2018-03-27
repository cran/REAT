\name{theil}
\alias{theil}

\title{
Theil inequality index
}

\description{
Calculating the Theil inequality index
}

\usage{
theil(x)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}

}
\details{
Since there are several Theil measures of inequality, this function uses the formulation from Stoermann (2009).
}

\value{
A single numeric value of the \emph{Theil inequality index} (\eqn{0 < TI < 1}).
}

\references{
Stoermann, W. (2009): \dQuote{Regionaloekonomik: Theorie und Politik}. Muenchen : Oldenbourg.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{herf}}, \code{\link{hoover}} 
}

\examples{
# Example from Stoermann (2009):
regincome <- c(10,10,10,20,50)
theil(regincome) 
# 0.2326302
}