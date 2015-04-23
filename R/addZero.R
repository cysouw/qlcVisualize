# add zeros in empty space around some points

addZero <- function(points
                    , density = 0.02
                    , grid = 10
                    , boundary = 0.1
                    , show = TRUE) {

  p <- xy.coords(points)
  k <- MASS::kde2d(p$x, p$y, n = grid)

  zeros <- which(k$z < density, arr.ind = TRUE)
  zeroX <- k$x[zeros[,1]]
  zeroY <- k$y[zeros[,2]]

  rX <- diff(range(p$x))*boundary
  rY <- diff(range(p$y))*boundary
  pXmin <- min(k$x)-rX
  pXmax <- max(k$x)+rX
  pYmin <- min(k$y)-rY
  pYmax <- max(k$y)+rY

  borderX <- c(  k$x
               , k$x
               , rep(pXmin, times = length(k$y))
               , rep(pXmax, times = length(k$y))
               )
  borderY <- c(  rep(pYmin, length(k$x))
               , rep(pYmax, length(k$x))
               , k$y
               , k$y
               )

  if (show) {
    plot(borderX, borderY, col = "blue")
    points(zeroX, zeroY, col = "red")
    points(points)
  } else {
    return(xy.coords(c(zeroX,borderX),c(zeroY,borderY)))
  }
}
