\name{limage}
\alias{limage}
\title{
Level-images for visualisation of nominal data with various levels.
}
\description{
A level-image ("l-image") is a counterpart of the base function \code{\link{image}} for nominal data with various levels. A matrix (or dataframe coerced as matrix) is visualised by showing the most frequent contents of the cells by colouring. There are various methods for ordering of rows and columns provided (like with \code{\link{heatmap}}).
}
\usage{
limage(x, col = rainbow(4), order = NULL, show.remaining = FALSE, cex.axis = 1, cex.legend = 1, cex.remaining = 1, font = "")
}
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{order}{
%%
}
  \item{show.remaining}{
%%     ~~Describe \code{show.remaining} here~~
}
  \item{cex.axis}{
%%     ~~Describe \code{cex.axis} here~~
}
  \item{cex.legend}{
%%     ~~Describe \code{cex.legend} here~~
}
  \item{cex.remaining}{
%%     ~~Describe \code{cex.remaining} here~~
}
  \item{font}{
%%     ~~Describe \code{font} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{image}} in base and \code{pimage} in the package \code{seriation}.
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
limage(x, col = heat.colors(5))
limage(x, col = list(b="red",e="blue"), show.remaining = T)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
