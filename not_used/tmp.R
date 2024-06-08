
  addHole <- function(point, window) {
    # project point
    point <- sf::st_sfc(sf::st_point(point), crs = 4326)
    point <- sf::st_transform(point, crs = crs)
    # combine with other points
    coor <- sf::st_coordinates(x)
    coor <- rbind(st_coordinates(point), coor)
    # make triangulation
    points <- spatstat.geom:::ppp(coor[,1], coor[,2], window = spatstat.geom::as.owin(window))
    dist <- spatstat.geom::delaunayDistance(points)
    # make hole aroung point
    hole <- spatstat.geom::convexhull.xy(coor[which(dist[1,] ==  1),])
    hole <- sf::st_as_sfc(hole)
    hole <- sf::st_buffer(hole, -expansion)
    # add hole to window
    both <- list(window$geom[[1]][[1]], hole[[1]][[1]])
    window <- sf::st_sfc(sf::st_polygon(both))
    sf::st_crs(window) <- crs
    return(window)
  }

center <- 10
coor <- st_coordinates(x)
points <- spatstat.geom:::ppp(coor[,1], coor[,2],
                              window = owin(range(coor[,1]), range(coor[,2])))
dist <- spatstat.geom::delaunayDistance(points)
hole <- spatstat.geom::convexhull.xy(coor[which(dist[center,] ==  1),])
hole <- sf::st_as_sfc(hole)
hole <- sf::st_buffer(hole, -1000)
st_crs(hole) <- 2397

box <- sf::st_as_sfc(sf::st_bbox(x), crs = 2397)
