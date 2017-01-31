\name{lm.beta}
\alias{lm.beta}

\title{
Beta regression coefficients
}
\description{
Calculating the standardized (beta) regression coefficients of linear models
}
\usage{
lm.beta(linmod, dummy.na = TRUE)
}

\arguments{
  \item{linmod}{
A \code{lm} object (linear regression model) with more than one independent variable
}
  \item{dummy.na}{
logical argument that indicates if dummy variables should be ignored when calculating the beta weights (default: \code{TRUE}). Note that beta weights of dummy variables do not make any sense
}
 
}

\details{
Standardized coefficients (beta coefficients) show how many standard deviations a dependent variable will change when the regarded independent variable is increased by a standard deviation. The \eqn{\beta} values are used in multiple linear regression models to compare the real effect (power) of the independent variables when they are measured in different units. Note that \eqn{\beta} values do not make any sense for dummy variables since they cannot change by a standard deviation.     
}
\value{
A \code{list} containing all independent variables and the corresponding standardized coefficients. 
}

\references{
Backhaus, K./Erichson, B./Plinke, W./Weiber, R. (2016): \dQuote{Multivariate Analysemethoden: Eine anwendungsorientierte Einfuehrung}. Berlin: Springer.
}

\author{
Thomas Wieland
}

\examples{
x1 <- runif(100)
x2 <- runif(100)
# random values for two independent variables (x1, x2)
y <- runif(100)
# random values for the dependent variable (y)
testmodel <- lm(y~x1+x2)
# OLS regression
summary(testmodel)
# summary
lm.beta(testmodel)
# beta coefficients
}
