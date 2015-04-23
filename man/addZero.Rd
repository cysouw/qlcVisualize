\name{addZero}
\alias{addZero}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addZero(points, density = 0.02, grid = 10, boundary = 0.1, show = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points}{
%%     ~~Describe \code{points} here~~
}
  \item{density}{
%%     ~~Describe \code{density} here~~
}
  \item{grid}{
%%     ~~Describe \code{grid} here~~
}
  \item{boundary}{
%%     ~~Describe \code{boundary} here~~
}
  \item{show}{
%%     ~~Describe \code{show} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (points, density = 0.02, grid = 10, boundary = 0.1, 
    show = TRUE) 
{
    p <- xy.coords(points)
    k <- MASS::kde2d(p$x, p$y, n = grid)
    zeros <- which(k$z < density, arr.ind = TRUE)
    zeroX <- k$x[zeros[, 1]]
    zeroY <- k$y[zeros[, 2]]
    rX <- diff(range(p$x)) * boundary
    rY <- diff(range(p$y)) * boundary
    pXmin <- min(k$x) - rX
    pXmax <- max(k$x) + rX
    pYmin <- min(k$y) - rY
    pYmax <- max(k$y) + rY
    borderX <- c(k$x, k$x, rep(pXmin, times = length(k$y)), rep(pXmax, 
        times = length(k$y)))
    borderY <- c(rep(pYmin, length(k$x)), rep(pYmax, length(k$x)), 
        k$y, k$y)
    if (show) {
        plot(borderX, borderY, col = "blue")
        points(zeroX, zeroY, col = "red")
        points(points)
    }
    else {
        return(xy.coords(c(zeroX, borderX), c(zeroY, borderY)))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
