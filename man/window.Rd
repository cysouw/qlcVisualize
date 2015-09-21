\name{window}
\alias{hullToOwin}
\alias{gadmToOwin}
\alias{mapsToOwin}
\title{
Producing windows of class "owin"
}
\description{
Different ways to easily produce windows of class "owin" from the package "spatstat" are presented here. These are used by \link{voronoi}.
}
\usage{
hullToOwin(points, shift, alpha)
mapsToOwin(country, database = "worldHires")
gadmToOwin(country, sub = NULL, level = 0)
}
\arguments{
  \item{points}{
Set of points that need a window around them. Two column matrix.
}
  \item{shift}{
The amount of space around the outer points at determining the window.
}
  \item{alpha}{
Parameter for the 'curviness': lower values show more detail. Passed internally to \link[alphahull]{ahull}.
}
  \item{country}{
Name of the country to obtain borders and turn them into an \code{\link[spatstat]{owin}} object needed for the function \code{\link{voronoi}}. For \code{mapsToOwin} check \code{\link[maps]{map}} how to specify the names. For \code{gadmToOwin}, check \code{\link[raster]{getData}}.
  }
  \item{database}{
Database as used by \code{\link[maps]{map}}.
  }
  \item{sub, level}{
Names for Subdivisions of countries as available in the GADM database. Read the help file \code{\link[raster]{getData}} for more information about GADM, the subdivisions and how to acces them.
  }
}
\details{
For \code{hullToOwin}, the function \link[alphahull]{ahull} is used to make a hull around the points. This is then converted to an "owin" window.

The functions \code{mapsToOwin} and \code{GadmToWin} use external topogaphic boundaries to produce windows.
}
\value{
All functions return an object of class \code{owin} from the package \code{spatstat}.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\note{
Includes code from code from Andrew Bevan, based on code from Dylan Beaudette, see \url{https://stat.ethz.ch/pipermail/r-sig-geo/2012-March/014409.html}.

The function \code{gadmToOwin} needs online access to download the data. The data is saved in the current working directory, and will not be downloaded again when it is already available there.
}
\examples{
# Boundary of the German state "Hessen"
# This will need to access the GADM database online
\dontrun{
boundary <- gadmToOwin("DEU","Hessen",1)}

# A window does not have to be continuous
\dontrun{
random <- mapsToOwin(c("Germany","Greece"))
plot(random, main = NULL)}

# hull around some points
# note influence of alpha and shift
data(hessen)

hull <- hullToOwin(hessen$villages, shift = 0.2, alpha = 1)
plot(hull)
points(hessen$villages)

hull <- hullToOwin(hessen$villages, shift = 0.1, alpha = 0.2)
plot(hull)
points(hessen$villages)

}
