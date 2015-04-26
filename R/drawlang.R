drawLang <- function( points, data
            , main = NULL
						, col = NULL
            , offset = FALSE
						, min.freq = 5
						, levels = c(41,45,51)
						, lambda = NULL
						, legend.position = "bottomleft"
						, cex = 0.7
						, font = NULL
						, file.out = NULL
						, ... ) {

	# preparations
  if (is.list(points)) {
    points <- cbind(points$x, points$y)
  }

  if (is.null(dim(data))) {
    data <- as.factor(data)
    data <- t(sapply(levels(data),function(x){as.numeric(data==x)}))
	}

  if (offset) {
    off <- mean(diff(sort(points[,1])))/2
  }

  # which words to include in graphic
  freq <- apply(data,1,function(x){sum(na.omit(x != 0))})
  freq[freq <= min.freq] <- NA
  data <- data[order(freq, decreasing = TRUE, na.last = NA), ]

  # points ignored because of low frequency words or NAs
  stats <- data
  stats[is.na(stats)] <- 0
  ignore <- which(colSums(stats) == 0)

	# repeat colors when necessary
  if (is.null(col)) { col <- "black" }
	col <- rep(col, times = ceiling(nrow(data)/length(col)))

  # zero coordinates to get nicer plotted areas
  zeros <- addZero(points, show = FALSE, ...)

	# open plotting device for saving file
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
    		, main = main
    		, xaxt = "n"
    		, yaxt = "n"
    		)

	# add large grey crosses for non-included data (low frequency)
	points( points[ignore,]
    			, col = "grey"
          , cex = .5
    			)

	# loop through words and make contours
	for (i in 1:nrow(data)) {

		# determine height
		h <- data[i,]
		h0 <- rep.int(0, times = length(zeros$x))

		# make countours
    if (is.null(lambda)) { lambda  <- NA }

		kriging <- Krig(x = cbind(c(points[,1], zeros$x),
                              c(points[,2], zeros$y))
						, Y = c(h, h0)
						, lambda = lambda
						)
		surface <- predictSurface(kriging)
		contour(surface
				, levels = max(stats)*levels/100
				, lwd = seq(0.5, 2, length.out = length(levels))
				, col = col[i]
				, drawlabels = FALSE
				, add = TRUE
				)

		# add symbols
    if (offset) {
      angle <- 2*pi*i/nrow(data)
      rotation <- c(sin(angle),cos(angle)) * off
    } else {
      rotation <- c(0,0)
    }

		points( points[data[i,] > 0, ] + rotation
				, pch = i-1
				, cex = 0.7
				, col = col[i]
				)
	}

	# add legend
	if (!is.null(legend)) {
		par(family = font)
		legend( legend.position
    				, pch = 1:nrow(data)-1
    				, legend = rownames(data)
    				, cex = cex
    				, col = col[1:nrow(data)]
    				)
		par(family = NULL)
	}

	# close plotting device when saving to file
	if (!is.null(file.out)) {
		dev.off()
		}
}
