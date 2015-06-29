semmap <- function( points, data
            , main = NULL
						, draw = 5
						, levels = c(41,45,51)
            , labels = NULL
            , cex = 0.7
						, col = NULL
        # offset of points, ignored for now
				#   , offset = FALSE
        # smoothing paramater for Krig
            , lambda = NULL
        # parameters for legend
            , legend = TRUE
						, position = "bottomleft"
						, size = 0.7
						, font = NULL
        # write to file
						, file.out = NULL
				# parameters passed through to function "boundary"
						, ... ) {

	# preparations
  if (is.list(points)) {
    points <- cbind(points$x, points$y)
  }

  if (is.null(dim(data))) {
    # make matrix from vector of forms
    data <- as.factor(data)
    data <- t(sapply(levels(data),function(x){as.numeric(data==x)}))
	} else {
    # if a matrix, check for numeric, and normalize between 0 and 1

	}

  if (nrow(points) != ncol(data)) {
    stop("Number of points does not correspond to the data supplied")
  }

  # which words to include in graphic
  if (is.numeric(draw)) {
    # drawing determined by minimum frequency, and ordered by frequency
    freq <- apply(data,1,function(x){sum(na.omit(x != 0))})
    freq[freq <= draw] <- NA
    ordered <- order(freq, decreasing = TRUE, na.last = NA)
    data <- data[ordered, , drop = FALSE]

  } else {
    # manually selected levels in order selected
    data <- data[draw, , drop = FALSE]

  }

  # points ignored because of low frequency words or NAs
  stats <- data
  stats[is.na(stats)] <- 0
  ignore <- which(colSums(stats) == 0)

	# repeat colors when necessary
  if (is.null(col)) { col <- "black" }
	col <- rep(col, times = ceiling(nrow(data)/length(col)))

  # zero coordinates to get nicer plotted areas
  zeros <- boundary(points, show = FALSE, ...)

	# open plotting device for saving file
	if (!is.null(file.out)) {
		quartz(	width = 6
				, height = 6
				, type = "pdf"
				, file = file.out
		)
	}

	# prepare plotting frame
	plot( c(points, zeros)
    		, type = "n"
    		, xlab = ""
    		, ylab = ""
    		, main = main
    		, xaxt = "n"
    		, yaxt = "n"
    		)

	# add grey points for non-included data (low frequency)
  # labels are always printed immediately, without specific colours
  if (is.null(labels)) {
    points( points[ignore, ,drop = FALSE]
            , col = "grey"
            , cex = cex
    )
  } else {
    if (length(labels) != nrow(points)) {
      stop("Number of labels is not equal to the number of points")
    }
    bw <- rep("black", times = nrow(points))
    bw[ignore] <- "grey"
    text( points
          , labels = labels
          , col = bw
          , cex = cex
    )
  }

	# loop through data rows and make contours
  # note that this will result in an errormessage
  # when there is nothing to draw
  if (nrow(data) == 0) {
    stop("No data available to draw contours. Consider changing parameter 'draw'")
  }
	for (i in 1:nrow(data)) {

		# determine height
		h <- data[i,]
		h0 <- rep.int(0, times = length(zeros$x))

		# make countours
    if (is.null(lambda)) { lambda  <- NA }

		kriging <- Krig(x = cbind(c(points[,1], zeros$x),
                              c(points[,2], zeros$y))
						, Y = c(h, h0)
            , give.warnings = FALSE
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

		# add rotation around center: ignored for now
    # if (offset) {
    #  angle <- 2*pi*i/nrow(data)
    #  off <- mean(diff(sort(points[,1])))/2
    #  rotation <- c(sin(angle),cos(angle)) * off
    # } else {
      rotation <- c(0,0)
    # }

    # add points when no labels are plotted
    if (is.null(labels)) {
  		points( points[data[i,] > 0, , drop = FALSE] + rotation
  				, pch = i-1
  				, cex = 0.7
  				, col = col[i]
  				)
    }
	}

	# add legend
	if (legend) {
		par(family = font)
		legend( position
    				, pch = 1:nrow(data)-1
    				, legend = rownames(data)
    				, cex = size
    				, col = col[1:nrow(data)]
    				)
		par(family = NULL)
	}

	# close plotting device when saving to file
	if (!is.null(file.out)) {
		dev.off()
		}
}
