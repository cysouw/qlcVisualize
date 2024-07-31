library(rnaturalearth)
land <- rnaturalearth::ne_download(scale = 50, type = "land", category = "physical")
landHiRes <- rnaturalearth::ne_download(scale = 10, type = "land", category = "physical")
islands <- rnaturalearth::ne_download(scale = 10, type = "minor_islands", category = "physical")

# glottolog data version 5, downloaded 29 Juli 2024
g <- read.csv("data_raw/glottolog_languoid/languoid.csv")
lang <- g[g$level == "language",]
lang <- lang[lang$bookkeeping != "True",]
lang <- lang[!is.na(lang$longitude),]

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

world <- sf::st_make_valid(all)

# crop greenland to allow for nicer pacific centered maps
world[434] <- st_crop(world[434], c(xmin = -73, ymin = 59, xmax = -30, ymax = 84))

save(world, file = "world.rda")
