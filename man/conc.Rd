\name{conc}
\alias{conc}

\title{
Concentration/inequality measures
}

\description{
Calculating a set of concentration/inequality measures
}

\usage{
conc(x, na.rm = TRUE)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}
  \item{na.rm}{
logical argument that indicates whether NA values should be excluded before computing results
}
}

\details{
This function is a convenient wrapper for all functions calculating concentration/inequality measures.
}

\value{
A \code{matrix} containing the concentration/inequality measures.
}


\author{
Thomas Wieland
}

\seealso{
\code{\link{gini}}, \code{\link{herf}}, \code{\link{hoover}}, \code{\link{theil}} 
}

\examples{
data(Automotive)
conc(Automotive$Turnover2008)
conc(Automotive[4:8])
}