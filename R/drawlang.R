drawLang <- function( name, points, data
						, col = "black"
						, min.freq = 5
						, levels = c(.41,.45,.51)
						, lambda = NA
						, legend.position = "bottomleft"
						, font = NULL
						, cex = 0.7
						, file.out = NULL
						, ... ) {

	# preparations
	if (!is.null(dim(data))) {
		data <- as.factor(data[,name])
	} else {
		data <- as.factor(data)
	}

	if (is.list(points)) {
		points <- cbind(points$x, points$y)
	}

	# which words to include in graphic
	freq <- sort( table(data), decreasing = TRUE)
	freq <- freq[freq >= min.freq]
	words <- names(freq)

	# ignored because of low frequency
	ignore <- unlist(sapply(words, function(x) {which(data == x)}))

	# zero coordinates to get nicer round areas
	zeros <- addZero(points, show = FALSE, ...)

	# repeat colors when necessary
	col <- rep(col, times = ceiling(length(words)/length(col)))

	# open plotting device
	if (!is.null(file.out)) {
		quartz(	width = 6
				, height = 6
				, type = "pdf"
				, file = file.out
		)
	}

	# prepare plotting frame
	plot( points
		, type = "n"
		, xlab = ""
		, ylab = ""
		, main = name
		, xaxt = "n"
		, yaxt = "n"
		)

	# add large grey crosses for non-included data (individual cases)
	points( points[-ignore,]
			, col = "grey"
			)

	# add small grey crosses for missing data
	points( points[is.na(data),]
			, col = "grey"
			, cex = .5
			, pch = 4
			)

	# loop through words and make contours
	for (i in 1:length(words)) {

		# determine height
		h <- as.numeric(data == words[i])
		h0 <- rep.int(0, times = length(zeros$x))

		# make countours
		kriging <- Krig(  x = cbind(c(points[,1], zeros$x), c(points[,2], zeros$y))
						, Y = c(h, h0)
						, lambda = lambda
						)
		surface <- predictSurface(kriging)
		contour(surface
				, levels = levels
				, lwd = seq(0.5, 2, length.out = length(levels))
				, col = col[i]
				, drawlabels = FALSE
				, add = TRUE
				)

		# add symbols
		points( points[data == words[i],]
				, pch = i-1
				, cex = 0.7
				, col = col[i]
				)
	}

	# add legend
	if (!is.null(legend)) {
		par(family = font)
		legend( legend.position
				, pch = 1:length(words)-1
				, legend = words
				, cex = cex
				, col = col[1:length(words)]
				)
		par(family = NULL)
	}

	# close plotting device
	if (!is.null(file.out)) {
		dev.off()
		}
}
