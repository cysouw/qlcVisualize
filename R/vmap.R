# ====================
# make a dirichlet/voronoi tessalation of points in a window
# this is just a convenience wrapper around "dirichlet" from spatstat
# ====================

voronoi <- function(points, window) {

  p <- spatstat::ppp(points[,1],points[,2],window = window)
  v <- spatstat::dirichlet(p)

  if (!is.null(attr(p, "rejects"))) {
    rejected <- cbind(attr(p, "rejects")$x, attr(p, "rejects")$y)
    index <- apply(rejected, 1, function(x) {
                which(points[,1] == x[1] & points[,2] == x[2])
             })
    attr(v, "rejects") <- unlist(index)
  }

	return(v)
}

# ====================
# plotting of a voronoi-map (v-map)
# default plotting of tessalations in spatstat is not easy to use with colour filling
# ====================

vmap <- function(tesselation, col = NULL, add = FALSE, border = "black", internal.border = "grey", lwd = 1) {

	if (!add) {
		plot(0,0
			, xlim = tesselation$window$xrange
			, ylim = tesselation$window$yrange
			, type = "n"
			, axes = FALSE
			, ann = FALSE
		)
	}

	tiles <- spatstat::tiles(tesselation)
	nr <- length(tiles)

	# repeat colors if necessary
	col <- rep(col, times = ceiling(nr/length(col)))

	# plot all tiles individually, to allow for separate colors
	for (i in 1:nr) {
		spatstat::plot.owin(tiles[[i]]
							, add = TRUE
							, col = col[i]
							, border = internal.border
							, lwd = lwd
							)
	}

	# add outer border
	spatstat::plot.owin(tesselation$window
						, add = TRUE
						, border = border
						, lwd = lwd
						)
}
