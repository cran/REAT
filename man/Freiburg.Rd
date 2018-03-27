\name{Freiburg}
\alias{Freiburg}
\docType{data}
\title{
Employment data in Freiburg and Germany
}
\description{
Dataset with industry-specific employment in Freiburg and Germany in the years 2008 and 2014
}
\usage{data("Freiburg")}
\format{
  A data frame with 9 observations on the following 8 variables.
  \describe{
    \item{\code{industry}}{a factor with levels for the regarded industry based on the German official economic statistics (WZ2008) }
    \item{\code{e_Freiburg2008}}{a numeric vector with industry-specific employment in Freiburg 2008}
    \item{\code{e_Freiburg2014}}{a numeric vector with industry-specific employment in Freiburg 2014}
    \item{\code{e_g_Freiburg_0814}}{a numeric vector containing the growth of industry-specific employment in Freiburg 2008-2014, percentage}
    \item{\code{e_Germany2008}}{a numeric vector with industry-specific employment in Germany 2008}
    \item{\code{e_Germany2014}}{a numeric vector with industry-specific employment in Germany 2014}
    \item{\code{e_g_Germany_0814}}{a numeric vector containing the growth of industry-specific employment in Germany 2008-2014, percentage}
    \item{\code{color}}{a factor containg colors (\code{blue}, \code{brown}, ...)}
  }
}

\source{
Statistische Aemter des Bundes und der Laender: Regionaldatenbank Deutschland, Tab. 254-74-4, own calculations
}

\examples{
data(Freiburg)
# Loads the data
growth(Freiburg$e_Freiburg2008, Freiburg$e_Freiburg2014, growth.type = "rate")
# Industry-specific growth rates for Freiburg 2008 to 2014
}
