\name{vmap}
\alias{vmap}
\alias{voronoimap}
\alias{voronoi}
\alias{gadmToOwin}
\alias{mapToOwin}
\title{
Plotting a Voronoi-map ("v-map")
}
\description{
A "voronoi-map" (voronoi-tesselation, also known as dirichlet tesselation) is frequently used in quantitative dialectology. This function is a convenience wrapper to easily produce dialect maps with voronoi tesselations. Also described here are a few helper functions to obtain geographical boundary, and to produce the tessalation.
}
\usage{
vmap(tesselation, col = NULL, add = FALSE
  , border = "black", internal.border = "grey", lwd = 1)

voronoi(points, window)
mapsToOwin(country, database = "worldHires")
gadmToOwin(country, sub = NULL, level = 0, path = "")
}
\arguments{
  \item{tesselation}{
Tessalation of class \code{\link[spatstat]{tess}} from the library \code{\link[spatstat]{spatstat}}. Can easily be produces by using the convenience function \code{voronoi} provided here.
}
  \item{col}{
Vector of colors for the filling of the tessalation. Is recycled when there are more tiles than colours. The order of the tiles is the same as the order of the points as specified in the function \code{\link{voronoi}}.
}
  \item{add}{
Add graphics to an existing plot
}
  \item{border}{
Colour of the outer border. Specifying \code{NA} removes the border.
}
  \item{internal.border}{
Colour of the inner borders. Specifying \code{NA} removes all borders.
}
  \item{lwd}{
Line width of all borders
}
  \item{point}{
Two-column matrix with all coordinates of the points to make a voronoi tessalation.
  }
  \item{window}{
Outer boundary for the voronoi tesselation. Should be in the form of an \code{\link[spatstat]{owin}} object. There are two helper functions provided here to get such object. Note that the function \code{\link{voronoi}} will give warnings if there are points outside of this window.
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
This code is almost completely based on functions from the \code{spatstat} package. For convenience, first some geographical boundaries can easily be accessed and converted for use in \code{spatstat}. Then a voronoi tessalation can be made (based on the function \code{\link[spatstat]{dirichlet}}, which in turn is based on \code{\link[deldir]{deldir}}). Finally, this tessalation can be plotted filled with different colours.

Any legends have to be added manually by using \code{\link[graphics]{legend}}, see examples below.
}
\value{
\code{gadmToOwin} and \code{mapsToOwin} return an object of class \code{owin} from the package \code{spatstat}.
\code{voronoi} returns a tesselation of the class \code{tess} from the package \code{spatstat}.
\code{vmap} plots a map.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\note{
The function \code{gadmToOwin} needs online access to download the data. The data is saved in the current working directory, and will not be downloaded again when it is already available there.
}
\examples{
# Boundary of the German state "Hessen"
# This will need to access the GADM database online
\dontrun{
hessen <- gadmToOwin("DEU","Hessen",1)}
# As an offline example, the result is included in the package
data(hessen)
plot(hessen)

# Does not have to be continuous
random <- mapsToOwin(c("Germany","Greece"))
plot(random)

# make a voronoi tessalation for some villages in hessen
data(villages)
plot(hessen); points(villages,cex=.3)

tessalation <- voronoi(villages, hessen)
plot(tessalation)

# make a resizable plot with easy colour specification
vmap(tessalation, col = rainbow(5), i = NA)
legend("bottomright",legend = c("a","b","c","d","e"),fill = rainbow(5))
}