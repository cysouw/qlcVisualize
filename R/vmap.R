# ====================
# turn polygons from "maps" into window for spatstat
# ====================

mapsToOwin <- function(country, database = "worldHires") {

  require(mapdata)
	raw <- maps::map(database = database
					, regions = country
					, plot = FALSE
					, fill = TRUE
					)

	cutoff <- which(is.na(raw$x))
	cutoff <- c( 0, cutoff, length(raw$x)+1 )

	coor <- cbind(raw$x, raw$y)

	result <- list()
	for (i in 1:length(raw$names)) {
		result[[i]] <-  coor[(cutoff[i]+1) : (cutoff[i+1]-1), ]
	}
	names(result) <- raw$names

	return( spatstat::owin(poly = result) )
}

# ====================
# turn data from GADM into windows for spatstat
# ====================

gadmToOwin <- function(country, sub = NULL, level = 0) {

	raw <- raster::getData("GADM", country = country, level = level)

	if (!is.null(sub)) {
		name_objects <- which(grepl("^NAME_", names(raw@data)))
		selection <- sapply(raw@data[name_objects], function(x) {
			which(grepl(sub,x))
			})
		raw <- raw[unlist(selection),]
	}

	return( maptools::as.owin.SpatialPolygons(raw) )

}

# ====================
# make a dirichlet/voronoi tessalation of points in a window
# this is just a convenience wrapper around "dirichlet" from spatstat
# ====================

voronoi <- function(points, window) {
	spatstat::dirichlet(spatstat::ppp(points[,1],points[,2],window = window))
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
	col <- rep(col, times = ceiling(nr/length(col)))
	for (i in 1:nr) {
		spatstat::plot.owin(tiles[[i]]
							, add = TRUE
							, col = col[i]
							, border = internal.border
							, lwd = lwd
							)
	}

	spatstat::plot.owin(tesselation$window
						, add = TRUE
						, border =  border
						, lwd = lwd
						)
}
