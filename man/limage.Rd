\name{limage}
\alias{limage}
\alias{levelimage}
\title{
Drawing multi-level images for visualisation of nominal data with various levels.
}
\description{
A multi-level image ("l-image") is a counterpart of the base function \code{\link{image}} for nominal data with various levels. A matrix (or dataframe coerced as matrix) is visualised by showing the most frequent contents of the cells by colouring. There are various methods for ordering of rows and columns provided (like with \code{\link{heatmap}}).
}
\usage{
limage(x, col = rainbow(4), order = NULL,
  show.remaining = FALSE, cex.axis = 1, cex.legend = 1, cex.remaining = 1,
  font = "", asp = 1, method = "hamming", control = NULL)
}
\arguments{
  \item{x}{
A matrix or dataframe with the data to be displayed. Rows are shown on the x-axis, columns on the y-axis, showing the row- and column-names in the display. All data in the whole matrix is interpreted as one large factor with different levels.
}
  \item{col}{
Colors to be used for the display. The number of colours specified here are used for the top most frequent phenomena in the data. All other are shown as 'others'. Optionally use \code{show.remaining} to show these others in the visualisation
}
  \item{order}{
How should rows and columns be ordered? By default the order of the data matrix \code{x} is used. Many possible orderings are available, see Details.
}
  \item{show.remaining}{
Logical: should all levels without color be shown inside the boxes?
}
  \item{cex.axis}{
Size of the row and columns names of \code{x}, shown as axis labels.
}
  \item{cex.legend}{
Size of the legend text.
}
  \item{cex.remaining}{
Size of the text in the boxes. Only shown when \code{show.remaining = TRUE}.
}
  \item{font}{
Font to be used in the plotting, can be necessary for unusual unicode symbols. Passed internally to \code{par(family)}.
}
  \item{asp}{
Aspect-ratio of the plotting of the boxes. By default the boxes are drawn square. Manually resizing the boxes by changing the plotting window can be acheived by setting \code{asp = NA}.
  }
  \item{method}{
Method used to determine similarity, passed to \link[qlcMatrix]{sim.obs}, which is used internally to determine the order of rows and columns, using the method chosen in \code{order}.
  }
  \item{control}{
List of options passed to \link[seriation]{seriate}.
  }
  \item{plot}{
By default, a plot is returned. When \code{FALSE}, nothing is plotted, but the re-ordering is returned.
  }
}
\details{
There are many different orderings implemented: \code{"pca"} and \code{"varimax"} use the second dimension of \link{prcomp} and \link{varimax} respectively. \code{"mds"} will use the first dimension of \link{cmdscale}.

Further, all methods as provided in the function \link[seriation]{seriate} can be called. Specifcally, \code{"R2E"} and \code{"MDS_angle"} seem worthwhile to try out. Any paramters for these methods can be passed using the option \link{control}.
}
\value{
A plot is returned by default. When \code{plot = FALSE}, a list is returned with the reordering of the rows and the columns.
}
\note{
Note that it is slightly confusing that the image is sort-of a transposed version of the data matrix (rows of the matrix are shown as horizontal lines in the graphic, and they are shown from bottom to top). This is standard practice though, also used in \link{image} and \link{heatmap}, so it is continued here.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
\code{\link{image}} in base and \link[seriation]{pimage} in the package \code{seriation}.
}
\examples{
# a simple data matrix
x <- matrix(letters[1:5],3,5)
x[2,3] <- x[1,4] <- NA
rownames(x) <- c("one", "two", "three")
colnames(x) <- 1:5
x

# some basic level-images
limage(x)
limage(x, col = heat.colors(5), asp = NA)
limage(x, col = list(b="red",e="blue"), show.remaining = T)

\dontrun{
# more interesting example, different "f" sounds in german dialects
# note that fonts might be problematic on some platforms
# plotting window should be made really large as well
data(dialects)
limage(dialects$data, col = rainbow(8), order = "R2E", cex.axis = 0.3, cex.legend = 0.7, show.remaining = T, cex.remaining = 0.2)

# get reordering of rows, to identify the group of words with "p-f" correspondences
limage(dialects$data, order = "R2E", plot = FALSE)
}
}
