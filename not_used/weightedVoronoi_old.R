weightedVoronoi_old <- function(x, y = NULL, weights = "equal", window = NULL, grouping = NULL,
                            concavity = 0.9, expansion = 0.05, crs = 23028, maxit = 3) {

  # ======
  # Points
  # ======

  crs <- sf::st_crs(crs)

  if (!is.null(y)) {
    # combine x and y values
    x <- data.frame(x, y)
  }
  if (!is(x, "sf")) {
    # turn coordinates into sf
    colnames(x) <- c("X","Y")
    x <- sf::st_as_sf(x, coords=c("X", "Y"), crs = 4326)
    x <- sf::st_transform(x, crs)
  } else {
    # respect crs from sf object, if provided
    if (!is.na(sf::st_crs(x))) {
      crs <- sf::st_crs(x)
    }
  }

  # =====================
  # Window help functions
  # =====================

  makeOwin <- function(window) {
    suppressWarnings(sf::st_crs(window) <- crs)
    w <- spatstat.geom::as.owin(window)
    return(w)
  }

  getConcaveWindow <- function(x) {
    w <- concaveman::concaveman(x, concavity = concavity)
    w <- makeOwin(w)
    coor <- sf::st_coordinates(x)
    r <- max(diff(range(coor[,1])),diff(range(coor[,2])))
    w <- spatstat.random::expand.owin(w, distance = r*expansion)
    return(w)
  }

  getWindow <- function(group) {
    ids <- which(grouping == group)
    if (length(ids) == 1) {
      w <- sf::st_as_sfc(sf::st_buffer(x[ids,], dist = expansion))
      w <- makeOwin(w)
    } else if (length(ids) == 2) {
      line <- sf::st_linestring(sf::st_coordinates(x[ids,]))
      w <- sf::st_sfc(sf::st_buffer(line, dist = expansion))
      w <- makeOwin(w)
    } else {
      w <- getConcaveWindow(x[ids,])
    }
    return(w)
  }

  # ======
  # Window
  # ======

  if (!is.null(window)) {

    # try to convert window to spatstat::owin
    if (is(window, "SpatVector")) {
      # when using geodata it will give class terra::SpatVector
      # conversion from SpatVector -> sf -> Spatial
      window <- sf::as_Spatial(sf::st_as_sf(window))
    }
    if (is(window, "Spatial")) {
      # from sp::Spatial to spatstat::owin
      window <- makeOwin(window)
    }
    if (is(window, "sf")) {
      window <- sf::st_make_valid(sf::st_set_precision(window, 1e6))
      window <- sf::st_transform(window, crs)
      filled <- sf::st_intersects(x, window, sparse = FALSE)
      window <- window[colSums(filled)>0, ]
      window <- makeOwin(sf::st_geometry(window))
    }

  } else {

    # construct window from points and grouping
    if (is.null(grouping)) {
      # no grouping: a single window around all points
      window <- getConcaveWindow(x)
    } else {
      # various windows for groups in grouping vector
      groups <- names(table(grouping))
      window <- sapply(groups, getWindow, simplify = FALSE)
      window <- spatstat.geom::as.solist(window)
      window <- spatstat.geom::union.owin(window)
    }
  }

  # =======
  # Voronoi
  # =======

  # conversion because using spatstat
  coor <- sf::st_coordinates(x)
  # put points in window
  pp <- spatstat.geom::ppp(coor[,1], coor[,2], window = window)
  # catch points outside window
  if (!is.null(attr(pp, "rejects"))) {
    warning("See attr(…, \"rejects\") to see which points lie outside", call. = FALSE)
    rejected <- cbind(attr(pp, "rejects")$x, attr(pp, "rejects")$y)
    index <- apply(rejected, 1, function(x) {
      which(coor[,1] == x[1] & coor[,2] == x[2])
    })
    index <- unlist(index)
  } else {
    index <- NULL
  }
  if (!is.null(index)){
    x <- x[-index,]
  }
  # voronoi in window
  tiles <- spatstat.geom::dirichlet(pp)

  # ===============
  # Prepare results
  # ===============

  # back to sf
  tiles <- sf::st_as_sfc(tiles)
  sf::st_crs(tiles) <- crs
  window <- sf::st_as_sfc(window)
  sf::st_crs(window) <- crs
  # prepare output
  result <- sf::st_sf(voronoi = tiles)
  result$points <- x
  result$window <- window
  sf::st_geometry(result) <- "voronoi"

  # =========
  # Cartogram
  # =========

  # using cartogramR
  if (!is.null(weights)) {
    # default to equally-sized polygons
    if (weights == "equal") {
      weights <- rep(1, times = nrow(x))
    }
    # cartogram
    result$weights = weights
    carto <- cartogramR::cartogramR(result, count = "weights", options = list(maxit = maxit))
    # make new window from cartogram
    valid_carto <- sf::st_make_valid(sf::st_set_precision(carto$cartogram, 1e6))
    new_window <- sf::st_union(valid_carto)
    # new points
    new_points <- data.frame(carto$final_centers)
    new_points <- sf::st_as_sf(new_points, coords=c(1,2), crs = crs)
    # store results
    result$weightedVoronoi <- valid_carto
    result$weightedPoints <- new_points
    result$weightedWindow <- new_window
    sf::st_geometry(result) <- "weightedMap"
  }

  # store points outside window in attr
  attr(result, "rejects") <- index

  return(result)
}

