\name{G.regions.industries}
\alias{G.regions.industries}
\docType{data}

\title{
Firms and employment data for German regions 2015
}

\description{
The dataset contains the industry-specific firm stock and employment in the German regions ("Bundeslaender") for 2015.
}

\usage{data("G.regions.industries")}
\format{
  A data frame with 272 observations on the following 9 variables.
  \describe{
    \item{\code{year}}{a numeric vector containing the related year}
    \item{\code{region}}{a factor containing the names of the German regions (Bundeslaender)}
    \item{\code{region_code}}{a factor containing the codes of the German regions (Bundeslaender)}
    \item{\code{ind_code}}{a factor containing the codes of the industries (WZ2008)}
    \item{\code{ind_name}}{a factor containing the names of the industries (WZ2008)}
    \item{\code{firms}}{a numeric vector containing the related number of firms}
    \item{\code{emp_all}}{a numeric vector containing the related number of employees (incl. self-employed)}
    \item{\code{pop}}{a numeric vector containing the related population}
    \item{\code{area_sqkm}}{a numeric vector containing the related region size (in sqkm)}
  }
}

\source{
Compiled from:

Statistisches Bundesamt (2019): Tab. 11111-0001 - Gebietsflaeche: Bundeslaender, Stichtag.

Statistisches Bundesamt (2019): Tab. 12411-0010 - Bevoelkerung: Bundeslaender, Stichtag.

Statistisches Bundesamt (2019): Tab. 13311-0002 - Erwerbstaetige, Arbeitnehmer, Selbstaendige und mithelfende Familienangehoerige (im Inland): Bundeslaender, Jahre, Wirtschaftszweige (Arbeitskreis "Erwerbstaetigenrechnung des Bundes und der Laender").

Statistisches Bundesamt (2019): Tab. 52111-0004 - Betriebe (Unternehmensregister-System): Bundeslaender, Jahre, Wirtschaftszweige (Abschnitte), Beschaeftigtengroessenklassen.
}


\examples{
data (G.regions.industries)

lqs <- locq2(e_ij = G.regions.industries$emp_all, 
G.regions.industries$ind_code, G.regions.industries$region_code, 
LQ.output = "df")
# output as data frame

lqs_sort <- lqs[order(lqs$LQ, decreasing = TRUE),]
# Sort decreasing by size of LQ

lqs_sort[1:5,]
}