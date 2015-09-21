\name{ah2sp}
\alias{ah2sp}
\title{
Turning ahull into spatialPolygon
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ah2sp(x, increment = 360, rnd = 10, proj4string = sp::CRS(as.character(NA)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{increment}{
%%     ~~Describe \code{increment} here~~
}
  \item{rnd}{
%%     ~~Describe \code{rnd} here~~
}
  \item{proj4string}{
%%     ~~Describe \code{proj4string} here~~
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
function (x, increment = 360, rnd = 10, proj4string = sp::CRS(as.character(NA)))
{
    xdf <- as.data.frame(x$arcs)
    xdf <- subset(xdf, xdf$r > 0)
    res <- NULL
    if (nrow(xdf) > 0) {
        linesj <- list()
        prevx <- NULL
        prevy <- NULL
        j <- 1
        for (i in 1:nrow(xdf)) {
            rowi <- xdf[i, ]
            v <- c(rowi$v.x, rowi$v.y)
            theta <- rowi$theta
            r <- rowi$r
            cc <- c(rowi$c1, rowi$c2)
            ipoints <- 2 + round(increment * (rowi$theta/2),
                0)
            angles <- alphahull::anglesArc(v, theta)
            seqang <- seq(angles[1], angles[2], length = ipoints)
            x <- round(cc[1] + r * cos(seqang), rnd)
            y <- round(cc[2] + r * sin(seqang), rnd)
            if (is.null(prevx)) {
                prevx <- x
                prevy <- y
            }
            else if (x[1] == round(prevx[length(prevx)], rnd) &&
                y[1] == round(prevy[length(prevy)], rnd)) {
                if (i == nrow(xdf)) {
                  prevx <- append(prevx, x[2:ipoints])
                  prevy <- append(prevy, y[2:ipoints])
                  prevx[length(prevx)] <- prevx[1]
                  prevy[length(prevy)] <- prevy[1]
                  coordsj <- cbind(prevx, prevy)
                  colnames(coordsj) <- NULL
                  linej <- sp::Line(coordsj)
                  linesj[[j]] <- sp::Lines(linej, ID = as.character(j))
                }
                else {
                  prevx <- append(prevx, x[2:ipoints])
                  prevy <- append(prevy, y[2:ipoints])
                }
            }
            else {
                prevx[length(prevx)] <- prevx[1]
                prevy[length(prevy)] <- prevy[1]
                coordsj <- cbind(prevx, prevy)
                colnames(coordsj) <- NULL
                linej <- sp::Line(coordsj)
                linesj[[j]] <- sp::Lines(linej, ID = as.character(j))
                j <- j + 1
                prevx <- NULL
                prevy <- NULL
            }
        }
        lspl <- sp::SpatialLines(linesj)
        lns <- slot(lspl, "lines")
        polys <- sapply(lns, function(x) {
            crds <- slot(slot(x, "Lines")[[1]], "coords")
            identical(crds[1, ], crds[nrow(crds), ])
        })
        polyssl <- lspl[polys]
        list_of_Lines <- slot(polyssl, "lines")
        sppolys <- sp::SpatialPolygons(list(sp::Polygons(lapply(list_of_Lines,
            function(x) {
                sp::Polygon(slot(slot(x, "Lines")[[1]], "coords"))
            }), ID = "1")), proj4string = proj4string)
        hid <- sapply(slot(sppolys, "polygons"), function(x) slot(x,
            "ID"))
        areas <- sapply(slot(sppolys, "polygons"), function(x) slot(x,
            "area"))
        df <- data.frame(hid, areas)
        names(df) <- c("HID", "Area")
        rownames(df) <- df$HID
        res <- sp::SpatialPolygonsDataFrame(sppolys, data = df)
        res <- res[which(res@data$Area > 0), ]
    }
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{"internal"}
