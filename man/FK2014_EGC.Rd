\name{FK2014_EGC}
\alias{FK2014_EGC}
\docType{data}

\title{
Fictional sample data of 42 firms
}

\description{
Dataset with 42 firms from 4 industries in 3 regions (fictional sample data from Farhauer/Kroell 2014)
}

\usage{data("FK2014_EGC")}
\format{
  A data frame with 42 observations on the following 5 variables.
  \describe{
    \item{\code{region}}{unique ID of the region}
    \item{\code{industry}}{name of the industry (German language)}
    \item{\code{firm}}{firm ID}
    \item{\code{emp_firm}}{each firm's no. of employees}
    \item{\code{emp_region}}{total employment of the region}
  }
}

\source{
Farhauer, O./Kroell, A. (2014): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.
}

\references{
Farhauer, O./Kroell, A. (2014): \dQuote{Standorttheorien: Regional- und Stadtoekonomik in Theorie und Praxis}. Wiesbaden : Springer.
}

\examples{
# Example from Farhauer/Kroell (2014):
data(FK2014_EGC)

ellison.c(FK2014_EGC$emp_firm, FK2014_EGC$industry, 
FK2014_EGC$region, FK2014_EGC$emp_region)
}