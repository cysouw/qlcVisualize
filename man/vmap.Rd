\name{vmap}
\alias{vmap}
\alias{voronoimap}
\alias{voronoi}
\title{
Plotting a Voronoi-map ("v-map")
}
\description{
A "voronoi-map" (voronoi-tesselation, also known as dirichlet tesselation) is used in quantitative dialectology. This function is a convenience wrapper to easily produce dialect maps with voronoi tesselations. Also described here are a helper functions to produce the tessalation.
}
\usage{
vmap(tesselation, col = NULL, add = FALSE,
  border = "black", internal.border = "grey", lwd = 1)

voronoi(points, window)
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
  \item{points}{
Two-column matrix with all coordinates of the points to make a voronoi tessalation.
  }
  \item{window}{
Outer boundary for the voronoi tesselation. Should be in the form of an \code{\link[spatstat]{owin}} object. There are two helper functions provided here to get such object. Note that the function \code{\link{voronoi}} will give warnings if there are points outside of this window.
  }
}
\details{
This code is almost completely based on functions from the \code{spatstat} package. For convenience, first some geographical boundaries can easily be accessed and converted for use in \code{spatstat}. Then a voronoi tessalation can be made (based on the function \code{\link[spatstat]{dirichlet}}, which in turn is based on \code{\link[deldir]{deldir}}). Finally, this tessalation can be plotted filled with different colours.

Any legends have to be added manually by using \code{\link[graphics]{legend}}, see examples below.

The function \code{voronoi} returns a warning when points are attested that lie outside of the specified border. For these points there is no polygon specified. Indices for the rejected points outside the border can be accessed by \code{attr(x, "rejects")}.
}
\value{
\code{voronoi} returns a tesselation of the class \code{tess} from the package \code{spatstat}. When points outside of the border are attested, the indices of these points are added to an attribute "rejects".
\code{vmap} plots a map.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\examples{
data(hessen)
plot(hessen$boundary)



# make a voronoi tessalation for some villages in hessen
data(hessen)
plot(hessen$boundary)
points(hessen$villages, cex = 0.3)

tessalation <- voronoi(hessen$villages, hessen$boundary)
plot(tessalation)

# make a resizable plot with random colour specification
vmap(tessalation, col = rainbow(5), i = NA)
legend("bottomright", legend = c("a","b","c","d","e"), fill = rainbow(5))

# use actual colors from data, using first feature from supplied data
# multiple levels cannot easily be shown
# consider \link{lmap} for more detail
d1 <- hessen$data[,1:3]
d1 <- d1[,1]/rowSums(d1)
vmap(tessalation, col = rgb(1, 1-d1, 1-d1))
text(hessen$villages,labels=hessen$data[,1],cex=.5)
legend("bottomright", legend = c("es mir", "mir es / other"),
  fill = c("red", "white"))

# Use distances to determine colour, as proposed by Heeringa (2004)
# Note that different methods to establish distances can lead to rather
# different results! Also try method = "euclidean"
d <- dist(hessen$data, method = "canberra")
cols <- heeringa(d)
vmap(tessalation, col = cols)

}
