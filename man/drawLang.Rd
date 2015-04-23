\name{drawLang}
\alias{drawLang}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
drawLang(name, points, data, col = "black", min.freq = 5, levels = c(0.41, 0.45, 0.51), lambda = NA, legend.position = "bottomleft", font = NULL, cex = 0.7, file.out = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{points}{
%%     ~~Describe \code{points} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{min.freq}{
%%     ~~Describe \code{min.freq} here~~
}
  \item{levels}{
%%     ~~Describe \code{levels} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
}
  \item{legend.position}{
%%     ~~Describe \code{legend.position} here~~
}
  \item{font}{
%%     ~~Describe \code{font} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{file.out}{
%%     ~~Describe \code{file.out} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (name, points, data, col = "black", min.freq = 5, levels = c(0.41, 
    0.45, 0.51), lambda = NA, legend.position = "bottomleft", 
    font = NULL, cex = 0.7, file.out = NULL, ...) 
{
    if (!is.null(dim(data))) {
        data <- as.factor(data[, name])
    }
    else {
        data <- as.factor(data)
    }
    if (is.list(points)) {
        points <- cbind(points$x, points$y)
    }
    freq <- sort(table(data), decreasing = TRUE)
    freq <- freq[freq >= min.freq]
    words <- names(freq)
    ignore <- unlist(sapply(words, function(x) {
        which(data == x)
    }))
    zeros <- addZero(points, show = FALSE, ...)
    col <- rep(col, times = ceiling(length(words)/length(col)))
    if (!is.null(file.out)) {
        quartz(width = 6, height = 6, type = "pdf", file = file.out)
    }
    plot(points, type = "n", xlab = "", ylab = "", main = name, 
        xaxt = "n", yaxt = "n")
    points(points[-ignore, ], col = "grey")
    points(points[is.na(data), ], col = "grey", cex = 0.5, pch = 4)
    for (i in 1:length(words)) {
        h <- as.numeric(data == words[i])
        h0 <- rep.int(0, times = length(zeros$x))
        kriging <- Krig(x = cbind(c(points[, 1], zeros$x), c(points[, 
            2], zeros$y)), Y = c(h, h0), lambda = lambda)
        surface <- predictSurface(kriging)
        contour(surface, levels = levels, lwd = seq(0.5, 2, length.out = length(levels)), 
            col = col[i], drawlabels = FALSE, add = TRUE)
        points(points[data == words[i], ], pch = i - 1, cex = 0.7, 
            col = col[i])
    }
    if (!is.null(legend)) {
        par(family = font)
        legend(legend.position, pch = 1:length(words) - 1, legend = words, 
            cex = cex, col = col[1:length(words)])
        par(family = NULL)
    }
    if (!is.null(file.out)) {
        dev.off()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
