# Visualisation in comparative linguistics

visualisation is used here to inspect data and formulate hypotheses and generalisations

## Factor map

a factor map is a visualisation for categorical data with various levels (nominal data, typically modelled as "factors" in R).

## Level map

a level map is a map with lines indicating groups.

## Weighted map


geodata::gadm
geodata::world

rnaturalearth::ne_coastline
rnaturalearth::ne_countries
rnaturalearth::ne_states

data(hessen)
v <- weightedMap(hessen$villages, expansion = 4000, crs = 2397, maxit = 10)
plot(v$weightedVoronoi)

groups <- rep("a", times = 157)
groups[157] <- "b"
groups[c(58,59)] <- "c"
groups[c(101, 102, 107)] <- "d"
v <- weightedMap(hessen$villages, grouping = groups, crs = 2397, maxit = 10)
plot(v$weightedVoronoi)


# load worldmap
library(rnaturalearth)
window <- rnaturalearth::ne_download(scale = 50, type = "land", category = "physical")
plot(window)

# load WALS data, example feature 13: "tone"
library(lingtypology)
w13 <- wals.feature("13A")
head(w13)

# try different projections
azimuth_equaldist <- "+proj=aeqd +lat_0=90 +lon_0=45"
mollweide_atlantic <- "+proj=moll +lon_0=11"
mollweide_pacific <- "+proj=moll +lon_0=162"

# make a weighted voronoi
# higher maxit improve the map, but might take very long
v <- weightedMap(w13$longitude, w13$latitude, window = window,
                      maxit = 3, crs = azimuth_equaldist))

# some coordinates from WALS lie outside land, just ignore for now
feature13 <- w13$`13A`[-v$outsideWindow]

# prepare colors
cols13 <- c("red", "grey", "orange")
names(cols13) <- names(table(feat))
cols13 <- cols13[c(2,3,1)]

# the map
plot(v$weightedVoronoi, col = cols13[feature13], border = NA)
plot(v$weightedWindow, border = "black", add = TRUE, lwd = 0.5)

# add legend
legend("bottomleft", legend = names(cols13), fill = cols13, cex = .7)

# ------------

library(cartogram)
library(sf)

test <- nrow(x2)
system.time(tmp <- weightedMap(x2[1:test,], window = w2, weights = NULL))
v2 <- st_cast(tmp$voronoi, "MULTIPOLYGON")
all <- st_sf(geometry = v2)
all$weights <- rep(1, times = test)
system.time(tmp <- cartogram_cont(all, weight = "weights", itermax = 10))

# ------------


v <- st_voronoi(x = st_union(st_centroid(x)))
v <- st_intersection(st_cast(v), window)
v <- st_join(x = st_sf(v), y = x, join=st_intersects)
v <- st_cast(v, "MULTIPOLYGON")

