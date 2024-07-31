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
