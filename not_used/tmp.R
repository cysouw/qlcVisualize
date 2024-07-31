library(rnaturalearth)
land <- rnaturalearth::ne_download(scale = 50, type = "land", category = "physical")
landHiRes <- rnaturalearth::ne_download(scale = 10, type = "land", category = "physical")
islands <- rnaturalearth::ne_download(scale = 10, type = "minor_islands", category = "physical")

library(lingtypology)
lang <- lingtypology::glottolog[glottolog$level == "language",]
lang <- lang[!is.na(lang$longitude),]
lang$longitude <- ifelse(lang$longitude > 180, -360 + lang$longitude, lang$longitude)

library(sf)
coor <- sf::st_as_sf(lang, coords=c("longitude", "latitude"), crs = 4326)

# which languages are on minor islands
filledIs <- sf::st_intersects(coor, islands, sparse = F)
langIs <- islands[colSums(filledIs) > 0,]

# which languages are on land, normal resolution
land <- sf::st_cast(land, "POLYGON")
filledLa <- sf::st_intersects(coor, land, sparse = F)
langLa <- land[colSums(filledLa) > 0,]

# which are leftover
leftover <- coor[rowSums(filledLa) == 0 & rowSums(filledIs) == 0, ]

# which of leftovers are on islands, high resolution
landHiRes <- sf::st_cast(landHiRes, "POLYGON")
filledHiRes <- sf::st_intersects(leftover, landHiRes, sparse = F)
tmp <- landHiRes[colSums(filledHiRes) > 0, ]
test <- sf::st_intersects(langLa, tmp, sparse = F)
langAdd <- tmp[colSums(test) == 0, ]

# which are still leftover
filledHiRes <- sf::st_intersects(leftover, langAdd, sparse = F)
leftover <- leftover[rowSums(filledHiRes) == 0, ]

# max distance to coast of 3km will lead to bubble extension
circles <- sf::st_buffer(leftover$geometry, dist = 3000)
circles <- sf::st_simplify(circles, dTolerance = 1e3)
all <- rbind(langIs, langLa, langAdd)$geometry
all <- sf::st_sfc(c(all, circles))
all <- sf::st_union(all)
all <- sf::st_cast(all, "POLYGON")
all <- sf::st_transform(all, crs = "+proj=moll +lon_0=11.5")

world <- sf::st_make_valid(all)


# ===

azimuth_equaldist <- "+proj=aeqd +lat_0=90 +lon_0=45"
mollweide_atlantic <- "+proj=moll +lon_0=11"
mollweide_pacific <- "+proj=moll +lon_0=162"

library(lingtypology)
wals <- wals.feature("13A")

# https://github.com/cldf/cookbook/tree/master/recipes/cldf_r
# https://raw.githubusercontent.com/cldf-datasets/wals/master/cldf/languages.csv

x <- wals$longitude
y <- wals$latitude
window <- land

#crs <- "+proj=moll +lon_0=11"
crs <- "+proj=aeqd +lat_0=90 +lon_0=45"
weights = "equal"
maxit = 10
method = "dcn"
concavity = 2
expansion = 10
weights = "equal"

# ===

crs <- .setCRS(x, window, crs)
points <- .setPoints(x, y, crs)
grouping = rep("1", times = length(points))
result <- list(points = points, grouping = grouping)
result$window <- .setWindow(window, crs)
result <- .checkWindow(result)
result$voronoi <- .makeVoronoi(result$points, result$window)
result$weights = .prepareWeights(weights, result)

# outdated

wals$glottocode[wals$glottocode == "poqo1257"] <- "poqo1253"
wals$glottocode[wals$glottocode == "mamc1234"] <- "mamm1241"
wals$glottocode[wals$glottocode == "tuka1247"] <- "tuka1248"
wals$glottocode[wals$glottocode == "bali1280"] <- "unea1237"

# ===

data(hessen)
x = hessen$villages
y = NULL
window = NULL

expansion = 4000
concavity = 2
crs = 2397
maxit = 2

crs <- .setCRS(x, window, crs)
points <- .setPoints(x, y, crs)
grouping = rep("1", times = length(points))
result <- list(points = points, grouping = grouping)
window <- .makeWindow(points, grouping, concavity, expansion, crs)
result$window <- window
result$voronoi <- .makeVoronoi(result, crs)


groups <- rep("a", times = 157)
groups[157] <- "b"
groups[c(58,59)] <- "c"
groups[c(101, 102, 107)] <- "d"
holes <- list(c(9, 50.5), c(9.6, 51.3), c(8.9, 51))
expansion <- 3000

window <- .makeWindow(points, groups, concavity, expansion, crs)
window <- .addHoles(points, window, holes, expansion, crs)
result$window <- window
result$voronoi <- .makeVoronoi(result, crs)

# =======

# download data
f = "83A"
wals <- lingtypology::wals.feature(f)

# assign colors
table(wals[,f])
# feature 1A
cols <- c("grey", "red", "orange", "cyan1", "blue")
order =  c(5, 4, 1, 3, 2) # order for legend
# feature 83A
cols <- c("grey", "blue", "red")
order =  c(2,3,1) # order for legend

w <- read.csv("not_used/languages.csv")
genus <- merge(wals, w, by.x = "wals.code", by.y = "ID", sort = FALSE)$Genus
weights <- as.numeric(1/table(genus)[genus])

# prepare map
v <- weightedMap(wals$longitude, wals$latitude,
                 window = window, crs = "+proj=moll +lon_0=11", maxit = 8, weights = weights)

# prepare feature
feature <- wals[,f][-v$outsideWindow]
names(cols) <- names(table(feature))

# plot map
plot(v$weightedVoronoi, col = cols[feature], border = NA)
plot(v$weightedWindow, border = "black", add = TRUE, lwd = 0.5)

# add legend
cols <- cols[order]
legend("bottomleft", legend = names(cols), fill = cols, cex = .7)

# ====

ph_all <- lingtypology::phoible.feature()
#ph <- ph_all[ph_all$source == "ph",]
ph <- ph_all

distr <- table(ph$inventoryid, ph$phoneme)
distr <- matrix(distr, ncol = ncol(distr), dimnames = dimnames(distr))

s <- qlcMatrix::cosSparse(t(distr), norm = norm1)
cols <- qlcVisualize::heeringa(as.dist(max(s)-s))

lang <- unique(ph[, c("glottocode", "inventoryid")])
lang <- merge(lang, glottolog, by = "glottocode")

sel <- !duplicated(lang$glottocode)

v <- weightedMap(lang$longitude[sel], lang$latitude[sel], window = land,
                 crs = mollweide_atlantic, maxit = 2, verbose = 1, method = "gsm")
plot(v$weightedVoronoi, col = cols[sel][-v$outsideWindow], border = NA)

# ===

f <- paste0(c(1:138)[-c(3,25,81,95,96,97)], "A")
wals <- lingtypology::wals.feature(f)
coverage <- apply(wals[,f],1,function(x){sum(!is.na(x))})
sel <- coverage > 40
data <- wals[sel, f]
s <- qlcMatrix::sim.obs(data, method = "hamming")
cols <- qlcVisualize::heeringa(as.dist(max(s)-s))

v <- weightedMap(wals$longitude[sel], wals$latitude[sel], window = window, crs = mollweide_atlantic, maxit=20)
plot(v$weightedVoronoi, col = cols[-v$outsideWindow], border = NA)

# ====

w <- read.csv("not_used/languages.csv")
genus <- merge(wals, w, by.x = "wals.code", by.y = "ID", sort = FALSE)$Genus
weights <- 1/table(genus)[genus]

# ====

boundary(st_coordinates(v$weightedPoints),
         grid = 50,
         density = 1e-15,
         box.offset = .05,
         tightness = 2e6)

lmap(st_coordinates(v$weightedPoints),
     wals[,feature],
     levels = c(.1,.2,.3,.4),
     grid = 40,
     density = 1e-15,
     box.offset = .05,
     tightness = 4e6)

# ====

library(sp)
library(sf)


n <- c(0, 0.5, 1)
names(n) <- c("No tones", "Simple tone system", "Complex tone system")
n <- n[wals[,feature]]

addContour <- function(heights, coords, window, crs,
                       levels = c(.4, .45, .5), col = "black", grid = 5e4) {

  # prepare data
  data <- st_sf(geometry = coords, feature = heights, crs = crs)

  # make grid inside polygons
  w <- sf::as_Spatial(window)
  grd <- as.data.frame(sp::spsample(w, "regular", n = grid))
  names(grd) <- c("X", "Y")
  grd <- sf::st_as_sf(grd, coords = c("X", "Y"), crs = crs)
  grd <- stars::st_rasterize(grd)

  # ordinary kriging
  m <- automap::autofitVariogram(feature ~ 1, data)
  k <- gstat::krige(feature ~ 1, data, newdata = grd, model = m$var_model)
  z <- k["var1.pred",,]

  # draw contour
  widths <- seq(.5, 2, length.out = length(levels))
  contour(z, add = TRUE, drawlabels = FALSE,
          levels = levels, lwd = widths, col = col)
}

b <- seq(-.1, 1, .1)
plot(k["var1.pred",,], breaks = b, col = hcl.colors(length(b)-1, 'Spectral'))

coor <- st_coordinates(k)
coor <- apply(coor, 2, as.integer)
grid <- table(coor[,1], coor[,2])
coor <- apply(coor, 2, as.character)
for (i in 1:nrow(coor)) {grid[coor[i,1],coor[i,2]] <- k$var1.pred[i]}
grid[grid== 0] <- NA

plot(v$weightedWindow, border = "grey")
coor <- sapply(dimnames(grid), as.numeric)
cont <- contour(coor[[1]], coor[[2]], grid, levels = 0.3, add = T, lwd = 2, drawlabels = F, col = "red")
