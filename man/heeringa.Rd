\name{heeringa}
\alias{heeringa}
\title{
Heeringa-style colours
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
heeringa(dist, power = 0.5, mapping = c(1, 2, 3))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{
\code{\link{dist}} object specifying distances between points.
}
  \item{power}{
Factor used to influence the results of the multidimensional scaling. Values closer to one will lead to clearer separated colours, while higher values will lead to more gradual colours.
  }
  \item{mapping}{
Optional vector to change the mapping of the dimensions to the colours. Should be of length 3, specifying to which color each of the three dimensions is mapped. A 1 means 'red', a 2 meand 'green' and a 3 means 'blue'. Adding a minus reverses the mapping.
}
}
\details{
This proposal goes back to Heeringa (2004). The idea is to visualize distances by mapping the first three dimensions of a multidimensional scaling to the the red-green-blue scales. The \code{mapping} vector can be used to change the mapping to the colours.
}
\value{
A vector of colours of the same length as the size of the dist object.
}
\references{
Heeringa, Wilbert. "Measuring Dialect Pronunciation Differences Using Levenshtein Distance." Ph.D. Thesis, Rijksuniversiteit Groningen, 2004.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\examples{
data(hessen)
tessalation <- voronoi(hessen$villages, hessen$boundary)
d <- dist(hessen$data, method = "canberra")

# different mappings of the colors
vmap(tessalation, d = d, power = 0.5)
vmap(tessalation, d = d, mapping = c(3, -2, 1))
}
