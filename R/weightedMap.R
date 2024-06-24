weightedMap <- function(x, y = NULL, window = NULL, crs = NULL, weights = "equal",
                        grouping = NULL, holes = NULL, add_out = FALSE,
                        concavity = 2, expansion = 1e3, maxit = 10, method = "dcn") {

  # set crs
  crs <- .setCRS(x, window, crs)
  # set points to proper sf format
  x <- .setPoints(x, y, crs)
  # prepare output
  result <- list(points = sf::st_geometry(x))

  # -----------
  # prepare window
  if (!is.null(window)) {
    # set provided window to proper sf format
    window <- .setWindow(window, crs)
    # check intersection of points and window
    filled <- sf::st_intersects(x, window, sparse = FALSE)
    # remove polygons from window that have no points
    empty <- which(colSums(filled) == 0)
    if (length(empty) > 0) {
      window <- window[-empty, ]
      # add to output
      result$emptyPolygons <- empty
      warning(
        "Some polygons do not contain any points and are removed, see $emptyPolygons.",
        call. = FALSE
        )
    }
    # remove points outside of the window
    outside <- which(rowSums(filled) == 0)
    if (length(outside) > 0) {
      if (add_out) {
        added <- .makeWindow(x[outside,], 1:length(outside), 1, 3*expansion, crs)
        added <- sf::st_cast(added, "MULTIPOLYGON")
        window <- sf::st_sf(geometry = c(window$geometry, added))
      } else {
        x <- x[-outside,]
        # add to output
        result$outsideWindow <- outside
        warning(
          "Some points lie outside of the window and are ignored, see $outsideWindow.",
          call. = FALSE
        )
      }
    }
    # make grouping from provided window
    grouping <- unlist(sf::st_intersects(x, window))
  } else {
    # without provided window, make new window around points
    # optionally using grouping
    if (is.null(grouping)) {
      grouping <- rep("1", times = nrow(x))
    }
    window <- .makeWindow(x, grouping, concavity, expansion, crs)
    # add holes inside window
    if (!is.null(holes)) {
      window <- .addHoles(x, window, holes, expansion, crs)
    }
  }
  # add window and grouping to output
  result$window <- sf::st_union(window)
  result$grouping <- grouping

  # ------------
  # make voronoi
  voronoi <- .makeVoronoi(x, window, crs)
  # add voronoi to output
  result$voronoi <- voronoi

  # --------------
  # make cartogram, but only when weights are provided
  if (!is.null(weights)) {
    # prepare weights
    if (length(weights) ==  1 && weights == "equal") {
      # default to equally-sized polygons
      weights <- rep(1, times = nrow(x))
    } else if (!is.null(window) && exists("outside") && length(outside > 0)) {
      # remove weights for points outside window
      weights <- weights[-outside]
    }
    # add weights to output
    result$weights = weights
    # make cartogram
    carto <- .makeCarto(x, voronoi, weights, window, grouping, maxit = maxit, method = method, crs)
    # repeat voronoi to regularize map
    voronoi <- .makeVoronoi(carto$centers, carto$polygons, crs)
    if (!is.null(carto$outside)) {
      weights <- weights[-carto$outside]
    }
    # add weighted map to output
    result$weightedPoints <- sf::st_geometry(carto$centers)
    result$weightedWindow <- sf::st_union(carto$polygons)
    result$weightedVoronoi <- voronoi # sf::st_cast(carto$polygons, "MULTIPOLYGON")
    result$problems <- carto$outside
  }
  return(result)
}

# shorter alternative name
wmap <- weightedMap

# ======
# setCRS
# ======

.setCRS <- function(x, window, crs) {
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
      warning("No crs provided. EPSG:3857 is assumed")
      crs <- 3857
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
  return(crs)
}

# =========
# setPoints
# =========

.setPoints <- function(x, y ,crs) {
  if (!is.null(y)) {
    # combine x and y values
    x <- data.frame(x, y)
  }
  if (!is(x, "sf")) {
    # turn coordinates into sf
    colnames(x) <- c("X","Y")
    x <- sf::st_as_sf(x, coords=c("X", "Y"), crs = 4326)
    x <- sf::st_transform(x, crs)
    x <- sf::st_jitter(x, factor = 10e-8)
  }
}

# =========
# setWindow
# =========

.setWindow <- function(window, crs) {
  # try to convert window from spatstat::owin
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
    window <- sf::st_cast(window, "MULTIPOLYGON")
  } else {
    stop("Provided window cannot be interpreted: try to use sf format")
  }
  return(window)
}

# ==========
# makeWindow
# ==========

.makeWindow <- function(x, grouping, concavity, expansion, crs) {
  # construct various windows for groups in grouping vector
  groups <- names(table(grouping))
  window <- sapply(groups, .getWindow, grouping = grouping, x = x
                   , concavity = concavity, expansion = expansion
                   , crs = crs, simplify = FALSE
  )
  window <- sf::st_sfc(sapply(window, sf::st_geometry))
  sf::st_crs(window) <- crs
  return(window)
}

.getWindow <- function(group, grouping, x, concavity, expansion, crs) {
  ids <- which(grouping == group)
  if (length(ids) == 1) {
    # circle
    w <- sf::st_geometry(sf::st_buffer(x[ids,], dist = expansion))
  } else if (length(ids) == 2) {
    # squirkle
    line <- sf::st_cast(sf::st_combine(x[ids,]), "LINESTRING")
    w <- sf::st_buffer(line, dist = expansion)
  } else {
    # concave window
    w <- .getConcaveWindow(x[ids,], concavity, expansion, crs)
    # test window: unexplicably sometimes a point falls outside
    filled <- sf::st_intersects(x[ids,], w, sparse = FALSE)
    # solution: just try again
    while (min(rowSums(filled)) == 0) {
      x <- sf::st_jitter(x)
      w <- .getConcaveWindow(x[ids,])
      filled <- sf::st_intersects(x[ids,], w, sparse = FALSE)
    }
  }
  return(w)
}

.getConcaveWindow <- function(x, concavity, expansion, crs) {
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

# ========
# addHoles
# ========

.addHoles <- function(x, window, holes, expansion, crs) {
  # combine points for holes
  points <- do.call(rbind, holes)
  points <- sf::st_multipoint(points)
  points <- sf::st_sfc(points, crs = 4326)
  points <- sf::st_cast(points, "POINT")
  # project points
  points <- sf::st_transform(points, crs = crs)
  # combine with other points
  coor <- sf::st_coordinates(x)
  coor <- rbind(sf::st_coordinates(points), coor)
  # get closest points
  w <- spatstat.geom::as.owin(window)
  points <- suppressWarnings(
    spatstat.geom::ppp(coor[,1], coor[,2], window = w)
  )
  dist <- spatstat.geom::delaunayDistance(points)
  # make holes
  allHoles <- sapply(1:length(holes), .makeHole
                     , coor = coor, d = dist, expansion = expansion, crs = crs
                     )
  allHoles <- do.call(c, allHoles)
  allHoles <-  sf::st_sfc(sf::st_union(sf::st_make_valid(allHoles)))
  sf::st_crs(allHoles) <- crs
  # combine holes with window
  window <- sf::st_sfc(sf::st_difference(window, allHoles))
  return(window)
}

.makeHole <- function(point, coor, d, expansion, crs) {
  # make holes inside closest points to provided hole-point
  hole <- spatstat.geom::convexhull.xy(coor[which(d[point,] ==  1),])
  hole <- sf::st_as_sfc(hole)
  sf::st_crs(hole) <- crs
  hole <- sf::st_buffer(hole, -2*expansion)
  hole <- sf::st_buffer(hole, expansion)
  return(hole)
}

# ===========
# makeVoronoi
# ===========

.makeVoronoi <- function(x, window, crs) {
  if (length(window) == 1) {
    voronoi <- .getVoronoi(x, window, crs)
  } else {
    # combine separate voronoi parts into one geometry
    distr <- sf::st_intersects(x, window, sparse = FALSE)
    separate <- sapply(1:ncol(distr), function(i) {
      .getVoronoi(x[which(distr[,i] != 0),], window[i,], crs)
    }, simplify = FALSE)
    voronoi <- do.call(c, separate)
  }
  # get voronoi in right order
  order <- unlist(sf::st_intersects(x, voronoi))
  voronoi <- voronoi[order,]
  #voronoi <- sf::st_make_valid(sf::st_set_precision(voronoi, 1e6))
  #voronoi <- sf::st_cast(voronoi, "MULTIPOLYGON")
  return(voronoi)
}

.getVoronoi <- function(x, window, crs) {
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

# =============
# makeCartogram
# =============

.makeCarto <- function(x, voronoi, weights, window, grouping, maxit, crs, method) {
  # using package cartogramR
  tmp <- sf::st_sf(voronoi = voronoi, weights = weights)
  sf::st_geometry(tmp) <- "voronoi"
  carto <- cartogramR::cartogramR(tmp, count = "weights", method = method
                                  , options = list(maxit = maxit))
  # make valid new points
  valid_points <- data.frame(carto$final_centers)
  valid_points <- sf::st_as_sf(valid_points, coords=c(1,2), crs = crs)
  # make valid polygons
  valid_carto <- sf::st_make_valid(sf::st_set_precision(carto$cartogram, 1e6))
  # add grouping to polygons
  groups <- names(table(grouping))
  w <- sapply(groups, function(i) {
    sf::st_union(valid_carto[grouping == i,])
  })
  w <- sf::st_sfc(w, crs = crs)
  # reset points when outside window
  filled <- sf::st_intersects(valid_points, w, sparse = FALSE)
  outside <- which(rowSums(filled) == 0)
  if (length(outside) > 0) {
    valid_points[outside, ] <- x[outside, ]
  }
  # sometimes even the original points lie outside the deformed window
  # find the closest point inside
  filled <- sf::st_intersects(valid_points, w, sparse = FALSE)
  outside <- which(rowSums(filled) == 0)
  if (length(outside) > 0) {
    for (p in outside) {
      near <- sf::st_nearest_points(valid_points[p, ], w)
      # sometimes the nearest point is just outside, because of rounding errors
      tmp <- sf::st_cast(near, "POINT")
      tmp <- (tmp[[2]]-tmp[[1]])*1.1 + tmp[[1]]
      sf::st_geometry(valid_points[p, ]) <- sf::st_sfc(sf::st_cast(tmp, "POINT"))
    }
  }
  # check if really all points are captured
  filled <- sf::st_intersects(valid_points, w, sparse = FALSE)
  outside <- which(rowSums(filled) == 0)
  if (length(outside) > 0) {
    warning("there are still points outside, see $problems", call. = FALSE)
    valid_carto <- valid_carto[-outside,]
    valid_points <- valid_points[-outside,]
  } else {
    outside <- NULL
  }
  return(list(polygons = w, centers = valid_points, outside = outside))
}
