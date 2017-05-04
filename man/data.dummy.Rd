\name{data.dummy}
\alias{data.dummy}

\title{
Creating dummy variables
}

\description{
This function creates a dataset of dummy variables based on an input character vector.
}

\usage{
data.dummy(x)
}

\arguments{
  \item{x}{
A character vector
}
}

\details{
This function transforms a character vector \code{x} with \eqn{c} characteristics to a set of \eqn{c} dummy variables whose column names corresponding to these characteristics marked with \dQuote{_DUMMY}.    
}

\value{
A \code{data.frame} with dummy variables corresponding to the levels of the input variable.
}

\references{
Greene, W. H. (2012): \dQuote{Econometric Analysis}. 7th edition. Harlow : Pearson.
}

\author{
Thomas Wieland
}

\examples{
charvec <- c("Peter", "Paul", "Peter", "Mary", "Peter", "Paul")
# Creates a vector with three names (Peter, Paul, Mary)
data.dummy(charvec)
# Returns a data frame with 3 dummy variables
# (Mary_DUMMY, Paul_DUMMY, Peter_DUMMY)
}

\note{
This function contains code from the authors' package MCI.
}