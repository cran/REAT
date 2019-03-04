\name{ripley}
\alias{ripley}

\title{
Ripley's K
}

\description{
Analyzing point clustering with Ripley's K function
}

\usage{
ripley(loc_df, loc_id, loc_lat, loc_lon, 
area, t.max, t.sep = 10, K.local = FALSE, 
ci.boot = FALSE, ci.alpha = 0.05, ciboot.samples = 100,
progmsg = FALSE, K.plot = TRUE, Kplot.func = "K",
plot.title = "Ripley's K", plotX = "t", 
plotY = paste(Kplot.func, "Observed vs. expected"), 
lcol.exp = "blue", lcol.emp = "red", lsize.exp = 1, 
ltype.exp = "solid", lsize.emp = 1, ltype.emp = "solid",
bg.col = "gray95", bgrid = TRUE, bgrid.col = "white", 
bgrid.size = 2, bgrid.type = "solid")
}

\arguments{
  \item{loc_df}{
A data frame containing the points
}
  \item{loc_id}{
Column containing the IDs of the points in the data frame \code{loc_df}
}
  \item{loc_lat}{
Column containing the latitudes of the points in the data frame \code{loc_df}
}
  \item{loc_lon}{
Column containing the longitudes of the points in the data frame \code{loc_df}
}
  \item{area}{
Total area of the regarded region
}
  \item{t.max}{
Maximum distance
}
  \item{t.sep}{
Number of distance intervals
}
  \item{K.local}{
Logical arguments that indicates whether local K values are computed or not
} 
  \item{ci.boot}{
Logical arguments that indicates whether bootstrap confidence intervals are computed or not
} 
  \item{ci.alpha}{
Significance level of the bootstrap confidence intervals
} 
  \item{ciboot.samples}{
No. of bootstrap samples
} 
  \item{progmsg}{
Logical argument: Printing progress messages or not
} 
  \item{K.plot}{
Logical argument: Plot K function or not
} 
  \item{Kplot.func}{
Which function has to be plotted? K function (\code{Kplot.func = "K"}), L function (\code{Kplot.func = "L"}) or H function (\code{Kplot.func = "H"})
} 
  \item{plot.title}{
If \code{K.plot = TRUE}: Plot title
} 
  \item{plotX}{
If \code{K.plot = TRUE}: name of the X axis
} 
  \item{plotY}{
If \code{K.plot = TRUE}: name of the Y axis
} 
  \item{lcol.exp}{
If \code{K.plot = TRUE}: color of the line representing the expected values
} 
  \item{lcol.emp}{
If \code{K.plot = TRUE}: color of the line representing the empirical values
} 
  \item{lsize.exp}{
If \code{K.plot = TRUE}: size of the line representing the expected values
} 
  \item{lsize.emp}{
If \code{K.plot = TRUE}: size of the line representing the empirical values
} 
  \item{ltype.exp}{
If \code{K.plot = TRUE}: type of the line representing the expected values
} 
  \item{ltype.emp}{
If \code{K.plot = TRUE}: type of the line representing the empirical values
} 
  \item{bg.col}{
if \code{lc = TRUE} (plot of Lorenz curve), \code{bg.col} defines the background color of the plot (default: \code{"gray95"})
}
  \item{bgrid}{
if \code{lc = TRUE} (plot of Lorenz curve), the logical argument \code{bgrid} defines if a grid is shown in the plot
}
  \item{bgrid.col}{
if \code{lc = TRUE} (plot of Lorenz curve) and \code{bgrid = TRUE} (background grid), \code{bgrid.col} defines the color of the background grid (default: "white")
}
  \item{bgrid.size}{
if \code{lc = TRUE} (plot of Lorenz curve) and \code{bgrid = TRUE} (background grid), \code{bgrid.size} defines the size of the background grid (default: 2)
}
  \item{bgrid.type}{
if \code{lc = TRUE} (plot of Lorenz curve) and \code{bgrid = TRUE} (background grid), \code{bgrid.type} defines the type of lines of the background grid (default: \code{"solid"})
}
}

\details{
Calculating and plotting of the K function and its derivations (L function, H function) and, optionally, bootstrap confidence intervals.
}

\value{
The function returns a \code{list} containing:
\item{K }{A \code{data.frame} containing the K/L/H/t values}
\item{K_local }{A \code{data.frame} containing the local K values (if stated)}
\item{local_ci }{A \code{data.frame} containing the local confidence intervals (if stated)}
}

\references{
Kiskowski, M.A./Hancock, J. F./Kenworthy, A. (2009): \dQuote{On the Use of Ripley's K-function and its Derivatives to Analyze Domain Size}. In: \emph{Biophysical Journal}, \bold{97}, 4, p. 1095-1103.

Krider, R. E./Putler, R. S. (2013): \dQuote{Which Birds of a Feather Flock Together? Clustering and Avoidance Patterns of Similar Retail Outlets}. In: \emph{Geographical Analysis}, \bold{45}, 2, p. 123-149.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{dist}}, \code{\link{dist.buf}}, \code{\link{dist.mat}}
}


\examples{
\dontrun{
data(GoettingenHealth1)
# general practitioners, psychotherapists and pharmacies

area_goe <- 1753000000
# area of Landkreis Goettingen (sqm)
area_nom <- 1267000000
# area of Landkreis Northeim (sqm)
area_gn <- area_goe+area_nom
sqrt(area_gn/pi)

# this takes some seconds
ripley(GoettingenHealth1[GoettingenHealth1$type == "phys_gen",], 
"location", "lat", "lon", area = area_gn, t.max = 30000, t.sep = 300)

ripley(GoettingenHealth1[GoettingenHealth1$type == "pharm",], 
"location", "lat", "lon", area = area_gn, t.max = 30000, t.sep = 300)

ripley(GoettingenHealth1[GoettingenHealth1$type == "psych",], 
"location", "lat", "lon", area = area_gn, t.max = 30000, t.sep = 300)
}
}