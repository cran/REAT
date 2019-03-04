\name{FK2014_EGK}
\alias{FK2014_EGK}
\docType{data}

\title{
Fictional sample data of 42 firms
}

\description{
Dataset with 42 firms from 4 industries in 3 regions (fictional sample data from Farhauer/Kroell 2014)
}

\usage{data("FK2014_EGK")}
\format{
  A data frame with 42 observations on the following 5 variables.
  \describe{
    \item{\code{Region}}{unique ID of the region}
    \item{\code{Branche}}{name of the industry (German language)}
    \item{\code{Betrieb}}{firm ID}
    \item{\code{Beschaeftigte}}{each firm's no. of employees}
    \item{\code{BeschRegion}}{total employment of the region}
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
data(FK2014_EGK)
ellison.c(FK2014_EGK$Beschaeftigte, FK2014_EGK$Branche, 
FK2014_EGK$Region, FK2014_EGK$BeschRegion)
}