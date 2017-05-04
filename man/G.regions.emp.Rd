\name{G.regions.emp}
\alias{G.regions.emp}
\docType{data}

\title{
Employment data for German regions 2008-2014
}

\description{
The dataset contains the industry-specific employment in the German region ("Bundeslaender") for the years 2008 to 2014.
}

\usage{data("G.regions.emp")}
\format{
  A data frame with 1428 observations on the following 4 variables.
  \describe{
    \item{\code{industry}}{a factor containing the industry (in German language, e.g. "Baugewerbe" = construction, "Handel, Gastgewerbe, Verkehr (G-I)" = retail, hospitality industry and transport industry)}
    \item{\code{region}}{a factor containing the names of the German regions (Bundeslaender)}
    \item{\code{year}}{a numeric vector containing the related year}
    \item{\code{emp}}{a numeric vector containing the related number of employees}
  }
}

\source{
Statistische Aemter des Bundes und der Laender, Regionaldatenbank (2017): Sozialversicherungspflichtig Beschaeftigte: Beschaeftigte am Arbeitsort nach Geschlecht, Nationalitaet und Wirtschaftszweigen (Beschaeftigungsstatistik der Bundesagentur fuer Arbeit) - Stichtag 30.06. - regionale Ebenen(Tab. 254-74-4-B). 
}

\references{
Statistische Aemter des Bundes und der Laender, Regionaldatenbank (2017): Sozialversicherungspflichtig Beschaeftigte: Beschaeftigte am Arbeitsort nach Geschlecht, Nationalitaet und Wirtschaftszweigen (Beschaeftigungsstatistik der Bundesagentur fuer Arbeit) - Stichtag 30.06. - regionale Ebenen(Tab. 254-74-4-B). 
}

\examples{
data(G.regions.emp)
# Concentration of construction industry in Germany
# based on 16 German regions (Bundeslaender) for the year 2008
construction2008 <- G.regions.emp[(G.regions.emp$industry == "Baugewerbe (F)" | 
G.regions.emp$industry == "Insgesamt") & G.regions.emp$year == "2008",]
# only data for construction industry (Baugewerbe) and all-over (Insgesamt)
# for the 16 German regions in the year 2008
construction2008 <- construction2008[construction2008$region != "Insgesamt",]
# delete all-over data for all industries
gini.conc(construction2008[construction2008$industry=="Baugewerbe (F)",]$emp, 
construction2008[construction2008$industry=="Insgesamt",]$emp)

# Concentration of financial industry in Germany 2008 vs. 2014
# based on 16 German regions (Bundeslaender) for 2008 and 2014
finance2008 <- G.regions.emp[(G.regions.emp$industry == 
"Erbringung von Finanz- und Vers.leistungen (K)" | 
G.regions.emp$industry == "Insgesamt") & G.regions.emp$year == "2008",]
finance2008 <- finance2008[finance2008$region != "Insgesamt",]
# delete all-over data for all industries
gini.conc(finance2008[finance2008$industry == 
"Erbringung von Finanz- und Vers.leistungen (K)",]$emp, 
finance2008[finance2008$industry=="Insgesamt",]$emp)
finance2014 <- G.regions.emp[(G.regions.emp$industry == 
"Erbringung von Finanz- und Vers.leistungen (K)" | G.regions.emp$industry ==
"Insgesamt") & G.regions.emp$year == "2014",]
finance2014 <- finance2014[finance2014$region != "Insgesamt",]
# delete all-over data for all industries
gini.conc(finance2014[finance2014$industry == 
"Erbringung von Finanz- und Vers.leistungen (K)",]$emp, 
finance2014[finance2014$industry=="Insgesamt",]$emp)
}