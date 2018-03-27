\name{EU28.emp}
\alias{EU28.emp}
\docType{data}
\title{
Eurostat national employment data 2004-2016
}

\description{
Employment data for EU countires 2004-2016 (Source: Eurostat)
}

\usage{data("EU28.emp")}
\format{
  A data frame with 3000 observations on the following 7 variables.
  \describe{
    \item{\code{unit}}{measuring unit: thousand persones (\code{THS_PER})}
    \item{\code{nace_r2}}{NACE industry classification}
    \item{\code{s_adj}}{Adjustement of data: Not seasonally adjusted data (\code{NSA})}
    \item{\code{na_item}}{a factor with levels \code{SAL_DC}}
    \item{\code{geo}}{NUTS nation code}
    \item{\code{time}}{year}
    \item{\code{emp1000}}{Industry-specific employment in thousand persons}
  }
}

\source{
Eurostat (2018): Breakdowns of GDP aggregates and employment data by main industries and asset classes, Tab. code namq_10_a10_e. \url{http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=namq_10_a10_e}. Own postprocessing.
}

\examples{
data(EU28.emp)
EU28.emp[EU28.emp$time == 2016,]
# only data for 2016
}