\name{data.index}
\alias{data.index}
\title{
Creating index values
}
\description{
Standardizing a variable as an index
}
\usage{
data.index(dataset, col_index, col_ref, value_ref)
}

\arguments{
  \item{dataset}{
regarded \code{data.frame}
}
  \item{col_index}{
column to be converted to index values
}
  \item{col_ref}{
column with the reference values of the index (e.g. years or months)
}
  \item{value_ref}{
value from the reference column which is the reference value (=100)
}
}

\value{
a \code{numeric vector} consisting of the indexed values of col_index
}

\author{
Thomas Wieland
}

\examples{
# Creating test data
year <- 2010:2015
values <- c(20,24,21,28,27,29)
timeseries <- data.frame(year, values)
data.index(timeseries, "values", "year", "2012")
# returns index values
valuesindex <- data.index(timeseries, "values", "year", "2012")
timeseries2 <- data.frame(timeseries, valuesindex)
# add index values to data
}

