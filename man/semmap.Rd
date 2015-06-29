\name{semmap}
\alias{semmap}
\title{
Drawing Semantic Maps
}
\description{
Semantic maps (Haspelmath 2003) are a visualisation of linguistic diversity. A semantic map shows a predefined configuration of functions/senses in two-dimensional space with an overlay of language-specific encoding of these functions/senses. This function tries to emulate the linguistic tradition in an automatic fashion with various options for visual
}
\usage{
semmap(points, data,
   main = NULL, draw = 5, levels = c(41, 45, 51),
   labels = NULL, cex = 0.7, col = NULL, lambda = NULL,
   legend = TRUE, position = "bottomleft", size = 0.7, font = NULL,
   file.out = NULL, ...)
}
\arguments{
  \item{points}{
  Coordinates of the functions/senses, either specified as a two-column matrix or as \code{\link{xy.coords}}.
}
  \item{data}{
  Language data to be plotted as overlay over the points. Either specified as a vector of language-specific forms, or as a numeric matrix with the forms as rows and the functions/senses as columns (the language-specific forms should be specified as rownames). The values in the matrix designate the occurrence of the forms, allowing for the encoding of frequency/typicality and of overlap of different forms being used in the same function.
}
  \item{main}{
  Title for the plot
}
  \item{draw}{
  Which forms to draw. Specifying a numeric value
}
  \item{levels}{
%%     ~~Describe \code{levels} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{position}{
%%     ~~Describe \code{position} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{font}{
%%     ~~Describe \code{font} here~~
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
function (points, data, main = NULL, draw = 5, levels = c(41,
    45, 51), labels = NULL, cex = 0.7, col = NULL, lambda = NULL,
    legend = TRUE, position = "bottomleft", size = 0.7, font = NULL,
    file.out = NULL, ...)
{
    if (is.list(points)) {
        points <- cbind(points$x, points$y)
    }
    if (is.null(dim(data))) {
        data <- as.factor(data)
        data <- t(sapply(levels(data), function(x) {
            as.numeric(data == x)
        }))
    }
    if (is.numeric(draw)) {
        freq <- apply(data, 1, function(x) {
            sum(na.omit(x != 0))
        })
        freq[freq <= draw] <- NA
        ordered <- order(freq, decreasing = TRUE, na.last = NA)
        data <- data[ordered, , drop = FALSE]
    }
    else {
        data <- data[draw, , drop = FALSE]
    }
    stats <- data
    stats[is.na(stats)] <- 0
    ignore <- which(colSums(stats) == 0)
    if (is.null(col)) {
        col <- "black"
    }
    col <- rep(col, times = ceiling(nrow(data)/length(col)))
    zeros <- addZero(points, show = FALSE, ...)
    if (!is.null(file.out)) {
        quartz(width = 6, height = 6, type = "pdf", file = file.out)
    }
    plot(c(points, zeros), type = "n", xlab = "", ylab = "",
        main = main, xaxt = "n", yaxt = "n")
    if (is.null(labels)) {
        points(points[ignore, , drop = FALSE], col = "grey",
            cex = cex)
    }
    else {
        if (length(labels) != nrow(points)) {
            stop("Number of labels is not equal to the number of points")
        }
        bw <- rep("black", times = nrow(points))
        bw[ignore] <- "grey"
        text(points, labels = labels, col = bw, cex = cex)
    }
    if (nrow(data) == 0) {
        stop("No data available to draw contours. Consider changing parameter 'draw'")
    }
    for (i in 1:nrow(data)) {
        h <- data[i, ]
        h0 <- rep.int(0, times = length(zeros$x))
        if (is.null(lambda)) {
            lambda <- NA
        }
        kriging <- Krig(x = cbind(c(points[, 1], zeros$x), c(points[,
            2], zeros$y)), Y = c(h, h0), give.warnings = FALSE,
            lambda = lambda)
        surface <- predictSurface(kriging)
        contour(surface, levels = max(stats) * levels/100, lwd = seq(0.5,
            2, length.out = length(levels)), col = col[i], drawlabels = FALSE,
            add = TRUE)
        rotation <- c(0, 0)
        if (is.null(labels)) {
            points(points[data[i, ] > 0, , drop = FALSE] + rotation,
                pch = i - 1, cex = 0.7, col = col[i])
        }
    }
    if (legend) {
        par(family = font)
        legend(position, pch = 1:nrow(data) - 1, legend = rownames(data),
            cex = size, col = col[1:nrow(data)])
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
