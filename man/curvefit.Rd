\name{curvefit}
\alias{curvefit}

\title{
Curve fitting
}

\description{
Curve fitting (similar to SPSS and Excel) 
}

\usage{
curvefit(x, y, y.max = NULL, extrapol = NULL, 
plot.curves = TRUE, pcol = "black", ptype = 19, psize = 1,
lin.col = "blue", pow.col = "green", exp.col = "orange", logi.col = "red",
plot.title = "Curve fitting", plot.legend = TRUE,
xlab = "x", ylab = "y", y.min = NULL, ..., output.results = TRUE)
}

\arguments{
  \item{x}{
a numeric vector containing the explanatory variable
}
  \item{y}{
a numeric vector containing the dependent variable
}
  \item{y.max}{
Optional: given maximum for the logistic regression function
}
  \item{extrapol}{
a single numeric value for how many x units the dependent variable y shall be extrapolated
}
  \item{plot.curves}{
Logical argument that indicates whether the curves shall be plotted or not
}
  \item{pcol}{
If \code{plot.curves = TRUE}: Point color
}
  \item{ptype}{
If \code{plot.curves = TRUE}: Point type (pch)
}
  \item{psize}{
If \code{plot.curves = TRUE}: Point size
}
  \item{lin.col}{
If \code{plot.curves = TRUE}: Color of linear regression line
}
  \item{pow.col}{
If \code{plot.curves = TRUE}: Color of power function regression line
}
  \item{exp.col}{
If \code{plot.curves = TRUE}: Color of exponential function regression line
}
  \item{logi.col}{
If \code{plot.curves = TRUE}: Color of logistic function regression line
}
  \item{plot.title}{
If \code{plot.curves = TRUE}: Plot title
}
  \item{plot.legend}{
If \code{plot.curves = TRUE}: Logical argument that indicates whether a legend is added to the plot or not
}
  \item{xlab}{
If \code{plot.curves = TRUE}: X axis label
}
  \item{ylab}{
If \code{plot.curves = TRUE}: Y axis label
}
  \item{y.min}{
Optional: Y axis minimum
}
  \item{\dots}{
Optional: other plot parameters
}
  \item{output.results}{
Logical argument that indicates whether the model results are shown or not
}
}

\details{
Curve fitting for a given independent and dependent variable (\eqn{y = f(x)}). Similar to curve fitting in SPSS or Excel. Fitting of nonlinear regression models (power, exponential, logistic) via intrinsically linear models (Rawlings et al. 1998).
}

\value{
A \code{data frame} containing the regression results (Parameters a and b, std. errors, t values, ...)
}


\references{
Rawlings, J. O./Pantula, S. G./Dickey, D. A. (1998): \dQuote{Applied Regression Analysis}. Springer. 2nd edition.
}

\author{
Thomas Wieland
}

\examples{
x <- 1:20
y <- 3-2*x
curvefit(x, y, plot.curves = TRUE)
# fit with plot
curvefit(x, y, extrapol=10, plot.curves = TRUE)
# fit and extrapolation with plot

x <- runif(20, min = 0, max = 100)
# some random data

# linear
y_resid <- runif(20, min = 0, max = 10)
# random residuals
y <- 3+(-0.112*x)+y_resid
curvefit(x, y)

# power
y_resid <- runif(20, min = 0.1, max = 0.2)
# random residuals
y <- 3*(x^-0.112)*y_resid
curvefit(x, y)

# exponential
y_resid <- runif(20, min = 0.1, max = 0.2)
# random residuals
y <- 3*exp(-0.112*x)*y_resid
curvefit(x, y)

# logistic
y_resid <- runif(20, min = 0.1, max = 0.2)
# random residuals
y <- 100/(1+exp(3+(-0.112*x)))*y_resid
curvefit(x, y)
}