\name{dialects}
\alias{dialects}
\docType{data}
\title{
Multiple correspondences of "f"-like sounds in German Dialects
}
\description{
In total 34 different words in which an f-like sound occurs. The differnt pronunciations of this sound in 183 different German villages are included in this dataset.
}
\usage{data(dialects)}
\format{
List of 2:
\describe{
\item{\code{villages}}{Dataframe with two variables LONGITUDE and LATITUDE for all 183 villages.}
\item{\code{data}}{Matrix with 34 columns showing the pronunciation in the 183 villages.}
}
}
\source{
Excerpt from \url{http://github.com/cysouw/PAD/}
}
\examples{
data(dialects)

maps::map("worldHires", "Germany", fill = TRUE, col = "grey90")
lmap(dialects$villages, dialects$data[,21], levels = c(0.20, 0.22, 0.24), add = TRUE, position = "topleft")
title(main = "f-sound in \'Kochlöffel\'")
}
\keyword{datasets}
