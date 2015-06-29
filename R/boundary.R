# determine points to set to zeros in empty space around some points

boundary <- function(points
                    , density = 0.02
                    , grid = 10
                    , box.offset = 0.1
                    , show = TRUE) {

  p <- xy.coords(points)
  k <- MASS::kde2d(p$x, p$y, n = grid)

  zeros <- which(k$z < density, arr.ind = TRUE)
  zeroX <- k$x[zeros[,1]]
  zeroY <- k$y[zeros[,2]]

  rX <- diff(range(p$x))*box.offset
  rY <- diff(range(p$y))*box.offset
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
    plot(borderX, borderY, col = "blue", pch = 19)
    contour(k, add = TRUE)
    contour(k, level = density, col = "red", add = TRUE)
    points(zeroX, zeroY, col = "red", pch =  19)
    points(points, pch = 20)
  } else {
    return(xy.coords(c(zeroX,borderX),c(zeroY,borderY)))
  }
}
