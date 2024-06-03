weightedMap <- function(x, y = NULL, window = NULL, crs = NULL,
                            weights = "equal", grouping = NULL, holes = NULL,
                            concavity = 2, expansion = 1000, maxit = 5) {

  # ===
  # crs
  # ===

  if (!is.null(crs)) {
    # use crs provided in call
    crs <- sf::st_crs(crs)
  } else {
    # respect crs from points or window
    if ((is(x, "sf") | is(x, "sfc")) && !is.na(sf::st_crs(x))) {
      crs_x <- sf::st_crs(x)
    } else {
      crs_x <- NULL
    }
    if ((is(window, "sf") | is(window, "sfc")) && !is.na(sf::st_crs(window))) {
      crs_window <- sf::st_crs(window)
    } else {
      crs_window <- NULL
    }
    if (is.null(crs_x) & is.null(crs_window)) {
      stop("No crs provided")
    } else if (!is.null(crs_x) & !is.null(crs_window)) {
      if (crs_x == crs_window) {
        crs <- crs_x
      } else {
        stop("crs from x and window are different")
      }
    } else if (!is.null(crs_x)) {
      crs <- crs_x
    } else {
      crs <- crs_window
    }
  }

  # ======
  # Points
  # ======

  if (!is.null(y)) {
    # combine x and y values
    x <- data.frame(x, y)
  }
  if (!is(x, "sf")) {
    # turn coordinates into sf
    colnames(x) <- c("X","Y")
    x <- sf::st_as_sf(x, coords=c("X", "Y"), crs = 4326)
    x <- sf::st_transform(x, crs)
    x <- sf::st_jitter(x)
  }

  # prepare output
  result <- list(points = sf::st_geometry(x))

  # =====================
  # Window help functions
  # =====================

  getConcaveWindow <- function(x) {
    # concaveman hull
    w <- concaveman::concaveman(x, concavity = concavity)
    w <- sf::st_transform(w, crs)
    # use spatstat::exand.owin to make nice expansion
    if (expansion > 0) {
      w <- spatstat.geom::as.owin(w)
      w <- spatstat.random::expand.owin(w, distance = expansion)
      w <- sf::st_as_sf(w)
      sf::st_crs(w) <- crs
    }
    return(sf::st_geometry(w))
  }

  getWindow <- function(group) {
    ids <- which(grouping == group)
    if (length(ids) == 1) {
      w <- sf::st_geometry(sf::st_buffer(x[ids,], dist = expansion))
    } else if (length(ids) == 2) {
      line <- sf::st_cast(sf::st_combine(x[ids,]), "LINESTRING")
      w <- sf::st_buffer(line, dist = expansion)
    } else {
      w <- getConcaveWindow(x[ids,])
    }
    return(w)
  }

  addHole <- function(point, window) {
    # project point
    point <- sf::st_sfc(sf::st_point(point), crs = 4326)
    point <- sf::st_transform(point, crs = crs)
    # combine with other points
    coor <- sf::st_coordinates(x)
    coor <- rbind(sf::st_coordinates(point), coor)
    # make triangulation
    points <- suppressWarnings(spatstat.geom:::ppp(coor[,1], coor[,2],
                                                   window = spatstat.geom::as.owin(window)))
    dist <- spatstat.geom::delaunayDistance(points)
    # make hole aroung point
    hole <- spatstat.geom::convexhull.xy(coor[which(dist[1,] ==  1),])
    hole <- sf::st_as_sfc(hole)
    sf::st_crs(hole) <- crs
    hole <- sf::st_buffer(hole, -2*expansion)
    hole <- sf::st_buffer(hole, expansion)
    # add hole to window
    window <- sf::st_sfc(sf::st_difference(window, hole))
    return(window)
  }

  # ======
  # Window
  # ======

  if (!is.null(window)) {

    # try to convert window to spatstat::owin
    if (is(window, "owin") | is(window, "SpatVector") | is(window, "Spatial")) {
      window <- sf::st_as_sf(window)
      if (is.na(sf::st_crs(window))) {
        sf::st_crs(window) <- 4326
      }
    }
    if (is(window, "sf")) {
      # make valid window
      window <- sf::st_make_valid(sf::st_set_precision(window, 1e6))
      window <- sf::st_transform(window, crs)
      filled <- sf::st_intersects(x, window, sparse = FALSE)
      # remove polygons in window that have no points
      empty <- which(colSums(filled) == 0)
      if (length(empty) > 0) {
        window <- window[-empty, ]
        result$emptyPolygons <- empty
        warning("Some polygons do not contain any points and are removed, see $emptyPolygons."
                , call. = FALSE)
      }
      # check for points outside of the window
      outside <- which(rowSums(filled) == 0)
      if (length(outside) > 0) {
        x <- x[-outside,]
        result$outsideWindow <- outside
        warning("Some points lie outside of the window and are ignored, see $outsideWindow."
                , call. = FALSE)
        }
    } else {
      stop("Provided window cannot be interpreted: try to use sf")
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
      window <- sf::st_sfc(sapply(window, sf::st_geometry))
      sf::st_crs(window) <- crs
    }
    # add holes
    if (!is.null(holes)) {
      for (hole in holes) {
        window <- addHole(hole, window)
      }
    }
  }

  # prepare output
  result$window <- sf::st_union(window)

  # =======
  # Voronoi
  # =======

  getVoronoi <- function(x, window) {
    if (nrow(x) == 1) {
      return(sf::st_geometry(window))
    } else {
      # this is hocus-pocus!
      v <- sf::st_voronoi(sf::st_union(x))
      v <- sf::st_intersection(sf::st_cast(v), window)
      v <- sf::st_join(x = sf::st_sf(v), y = x, join=sf::st_intersects)
      v <- sf::st_cast(v, "MULTIPOLYGON")
      # union of individual voronoi elements
      v <- sapply(sf::st_geometry(v), sf::st_union, simplify = FALSE)
      return(sf::st_sfc(v, crs = crs))
    }
  }

  if (length(window) == 1) {
    voronoi <- getVoronoi(x, window)
  } else {
    # combine separate voronoi parts into one geometry
    distr <- sf::st_intersects(x, window, sparse = FALSE)
    separate <- sapply(1:ncol(distr), function(i) {
      getVoronoi(x[which(distr[,i] != 0),],window[i,])
      }, simplify = FALSE)
    combine <- do.call(c, separate)
    order <- unlist(sf::st_intersects(x, combine))
    voronoi <- combine[order,]
  }

  # prepare output
  result$voronoi <- voronoi

  # =========
  # Cartogram
  # =========

  # using cartogramR
  if (!is.null(weights)) {
    if (length(weights) ==  1 && weights == "equal") {
      # default to equally-sized polygons
      weights <- rep(1, times = nrow(x))
    } else if (!is.null(window) && length(outside > 0)) {
      # possibly remove weights for points outside window
      weights <- weights[-outside]
    }
    result$weights = weights
    # cartogram
    tmp <- sf::st_sf(voronoi = result$voronoi, weights = weights)
    sf::st_geometry(tmp) <- "voronoi"
    carto <- cartogramR::cartogramR(tmp, count = "weights", options = list(maxit = maxit))
    # make new window from cartogram
    valid_carto <- sf::st_make_valid(sf::st_set_precision(carto$cartogram, 1e6))
    new_window <- sf::st_union(valid_carto)
    # new points
    new_points <- data.frame(carto$final_centers)
    new_points <- sf::st_geometry(sf::st_as_sf(new_points, coords=c(1,2), crs = crs))
    # store results
    result$weightedPoints <- new_points
    result$weightedWindow <- new_window
    result$weightedVoronoi <- sf::st_cast(valid_carto, "MULTIPOLYGON")
  }

  return(result)
}

# shorter alternative name
wmap <- weightedMap
