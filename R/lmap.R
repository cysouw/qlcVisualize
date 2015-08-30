lmap <- function( points, data
            , main = NULL
						, draw = 5
						, levels = c(0.41, 0.46, 0.51)
            , labels = NULL
            , cex = 0.7
						, col = rainbow(draw)
						, add = FALSE
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
				    , add.zeros = TRUE
						, ...
				) {

  # ============
	# preparations
  # ============

  if (is.data.frame(points)) {
    points <- as.matrix(points)
  }

  # treat data as matrix: points x levels
  if (is.null(dim(data))) {
    # make matrix from vector of forms
    data <- as.factor(data)
    data <- sapply(levels(data),function(x){as.numeric(data==x)})
	} else {
    # if a matrix, check for numeric
    data <- as.matrix(data)
    if (!is.numeric(data)) {
      stop("Matrix or dataframe can only contain numerical values")
    }
    # and normalize between 0 and 1: each point (row) adds up to 1
    data <- data/rowSums(data, na.rm = TRUE)
	}

  # check size of points and data
  if (nrow(points) != nrow(data)) {
    stop("Number of points does not correspond to the data supplied")
  }

  # points ignored because of no data
  ignore <- rowSums(data, na.rm = TRUE) == 0

  # which words to include in graphic
  if (is.numeric(draw) & length(draw) == 1) {
    # only most frequent levels are included
    freq <- colSums(data, na.rm = TRUE)
    ordered <- order(freq, decreasing = TRUE, na.last = NA)
    selection <- na.omit(ordered[1:draw])
  } else if (is.numeric(draw)) {
    selection <- draw
  } else {
    # manually selected levels in order selected
    selection <- sapply(draw,function(x){which(colnames(data)==x)})
  }
  # the rest is added as "other" in the last column
  nr.levels <- length(selection)
  other <- rowSums(data[ , -selection, drop = FALSE])
  data <- data[ , selection, drop = FALSE]
  data <- cbind(data, other = other)

  # check for multi-valued data
  maxrow <- apply(data[!ignore, 1:nr.levels, drop = FALSE], 1, max, na.rm = TRUE)
  single.valued <- sum(maxrow !=  1 & maxrow != 0) == 0

	# set grey colors when "col = NULL"
  if (!is.null(col)) {
    # check number of colors
    if (length(col) != length(selection)) {
      stop("Number of colors specified at 'col' should be the same as number of levels to plot specified at 'draw'")
    }
    # add grey for other levels
    col <- c(col, "grey")
  }
  if (is.null(col) & single.valued) {
    col <- rep("black", times = nr.levels)
    # others get color grey
    col <- c(col, "grey")
  }
  if (is.null(col) & !single.valued) {
    # others get color white "grey(1)"
    col <- grey(0:(nr.levels+1)/(nr.levels+1))
  }

  # add zero coordinates to get nicer plotted areas
	if (add.zeros) {
    zeros <- boundary(points, show = FALSE, ...)
	} else {
	  zeros <- NULL
	}

	# ========
	# plotting
	# ========

	if (!add) {

	  # open plotting device for saving file
  	if (!is.null(file.out)) {
  		quartz(	width = 6
  				, height = 6
  				, type = "pdf"
  				, file = file.out
  		)
  	}

  	# prepare plotting frame
  	plot( rbind(points, zeros)
      		, type = "n"
      		, xlab = ""
      		, ylab = ""
      		, main = main
      		, xaxt = "n"
      		, yaxt = "n"
      		)
	}

	# =============
	# make contours
  # using package "fields"
	# =============

	for (i in 1:nr.levels) {

	  # determine height
	  h <- data[,i]
	  h0 <- rep.int(0, times = nrow(zeros))

	  # make countours
	  if (is.null(lambda)) { lambda  <- NA }

	  kriging <- fields::Krig(x = rbind(points, zeros)
	                  , Y = c(h, h0)
	                  , give.warnings = FALSE
	                  , lambda = lambda
	  )
	  surface <- fields::predictSurface(kriging)
	  contour(surface
	          , levels = levels
	          , lwd = rev(seq(2, 0.5, length.out = length(levels)))
	          , col = col[i]
	          , drawlabels = FALSE
	          , add = TRUE
	  )
	}

	# ==========
	# add legend
	# ==========

	if (legend) {
	  par(family = font)
	  if (single.valued) {
	    legend( position
	            , pch = c(1:nr.levels,0)
	            , legend = colnames(data)
	            , cex = size
	            , col = col
	    )
	  } else {
	    legend( position
	            , fill = col
	            , legend = colnames(data)
	            , cex = size
	    )
	  }
	  par(family = NULL)
	}

	# =============================
  # plot labels or points or pies
	# =============================

  if (is.null(labels)) {
    # plot points/pies

    # add small grey points for points without data
    points( points[ignore, ,drop = FALSE]
            , col = "grey"
            , cex = cex/2
          )

    if (single.valued) {
      # plotting symbols if maximally one symbol per point
      for (i in 1:nr.levels) {
        points( points[data[,i] > 0, , drop = FALSE]
                , pch = i
                , cex = cex
                , col = col[i]
               )
      }
      if (ncol(data)>nr.levels) {
        points( points[data[,nr.levels+1] > 0, , drop = FALSE]
                , pch = 0
                , cex = cex
                , col = col[nr.levels+1])
      }
    } else {
      # otherwise plot pies. This implementation is slow!!!
      mapplots::draw.pie( x = points[!ignore,1]
                          , y = points[!ignore,2]
                          , z = data[!ignore,]
                          , radius = cex
                          , col = col
                          )
    }
  } else {
    # plot labels

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

	# close plotting device when saving to file
	if (!is.null(file.out)) {
		dev.off()
		}
}
