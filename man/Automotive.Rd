\name{Automotive}
\alias{Automotive}
\docType{data}
\title{
Automotive industry data
}

\description{
Top 20 automotive industry companies, including their manufacturing quantity and turnovers (Table from wikipedia)
}

\usage{data("Automotive")}
\format{
  A data frame with 20 observations on the following 8 variables.
  \describe{
    \item{\code{Rank}}{Rank of the company}
    \item{\code{Company}}{Name of the company (German)}
    \item{\code{Country}}{Origin county of the company (German)}
    \item{\code{Quantity2014}}{Quantity of produced vehicles in 2014}
    \item{\code{Quantity2014_car}}{Quantity of produced cars in 2014}
    \item{\code{Turnover2008}}{Annual turnover 2008 (in billion dollars)}
    \item{\code{Turnover2012}}{Annual turnover 2012 (in billion dollars)}
    \item{\code{Turnover2013}}{Annual turnover 2013 (in billion dollars)}
  }
}


\source{
Wikipedia (2018): \dQuote{Automobilindustrie --- Wikipedia{,} Die freie Enzyklopaedie}. \url{https://de.wikipedia.org/wiki/Automobilindustrie} (accessed October 14, 2018). Own postprocessing.
}

\references{
Wikipedia (2018): \dQuote{Automobilindustrie --- Wikipedia{,} Die freie Enzyklopaedie}. \url{https://de.wikipedia.org/wiki/Automobilindustrie} (accessed October 14, 2018). 
}

\examples{
# Market concentration in automotive industry

data(Automotive)

gini(Automotive$Turnover2008, lsize=1, lc=TRUE, le.col = "black", 
lc.col = "orange", lcx = "Shares of companies", lcy = "Shares of turnover / cars", 
lctitle = "Automotive industry: market concentration", 
lcg = TRUE, lcgn = TRUE, lcg.caption = "Turnover 2008:", lcg.lab.x = 0, lcg.lab.y = 1)
# Gini coefficient and Lorenz curve for turnover 2008

gini(Automotive$Turnover2013, lsize=1, lc = TRUE, add.lc = TRUE, lc.col = "red", 
lcg = TRUE, lcgn = TRUE, lcg.caption = "Turnover 2013:", lcg.lab.x = 0, lcg.lab.y = 0.85)
# Adding Gini coefficient and Lorenz curve for turnover 2013

gini(Automotive$Quantity2014_car, lsize=1, lc = TRUE, add.lc = TRUE, lc.col = "blue", 
lcg = TRUE, lcgn = TRUE, lcg.caption = "Cars 2014:", lcg.lab.x = 0, lcg.lab.y = 0.7)
# Adding Gini coefficient and Lorenz curve for cars 2014
}