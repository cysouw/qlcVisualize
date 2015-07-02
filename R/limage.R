limage <- function( x
                  , col = rainbow(4)
                  , order = NULL
          				, show.remaining = FALSE
          				, cex.axis = 1
          				, cex.legend = 1
          				, cex.remaining = 1
          				, font = ""
                  , method = "hamming"
                  , control = NULL
			          	) {

  # === reordering data ===

  x <- as.matrix(x)

  if (!is.null(order)) {
    sim.cols <- as.matrix(qlcMatrix::sim.obs(t(x), method = method))
    sim.rows <- as.matrix(qlcMatrix::sim.obs(x, method = method))

    if (order == "pca") {

      # PCA on similarities (aka "correspondence analysis")
      # use second dimension for ordering
      order.cols <- order(prcomp(sim.cols)$x[,2])
      order.rows <- order(prcomp(sim.rows)$x[,2])

    } else if (order == "varimax") {

      # varimax rotations on PCA for ordering of correspondences
      # use second dimension
      order.cols <- order(varimax(prcomp(sim.cols)$x)$loadings[,2])
      order.rows <- order(varimax(prcomp(sim.rows)$x)$loadings[,2])

    } else if (order == "mds") {

      # classic MDS
      # use first dimension for ordering
      order.cols <- order(cmdscale(max(sim.cols)-sim.cols)[,1])
      order.rows <- order(cmdscale(max(sim.rows)-sim.rows)[,1])

    } else {

      # use library seriation for ordering
      # option "R2E" works nice for getting groups of data
      order.cols <- seriation::get_order(seriation::seriate(
                              as.dist(max(sim.cols)-sim.cols)
                              , method = order
                              , control =  control
                            ))
      order.rows <- seriation::get_order(seriation::seriate(
                              as.dist(max(sim.rows)-sim.rows)
                              , method = order
                              , control = control
                            ))
    }
    x <- x[order.rows, order.cols]
  }

	# === plotting windows ===

	plot.new()
	op <- par(mar = c(5, 4, 4, 4) + 0.1
		        , family = font
		        )
	plot.window(xlim=c(0,dim(x)[1])
				    , ylim=c(0,dim(x)[2])
				)

	# === axes ===

	rect(0, 0, nrow(x), ncol(x))
	axis(1
		, at = c(1:dim(x)[1]) - 0.5
		, labels = rownames(x)
		, tick = FALSE
		, las = 2
		, cex.axis = cex.axis
		, mgp = c(3,0,0)
		)
	axis(2
		, at = c(1:dim(x)[2]) - 0.5
		, labels = colnames(x)
		, tick = FALSE
		, las = 2
		, cex.axis = cex.axis
		, mgp = c(3,0,0)
		)

	# === show missing data	===

	points(which(is.na(x), arr.ind = T) - 0.5
			, pch = 20
			, col = "grey"
			, cex = cex.remaining
			)

	# === selecting levels to plot ===

	all <- names(sort(table(as.vector(x)),decreasing = T))

	if (is.list(col)) {
		levs <- names(col)
		col <- unlist(col)
	} else {
		levs <- all
		if (length(levs) > length(col)) {
			levs <- levs[1:length(col)]
		} else {
			col <- col[1:length(levs)]
		}
		names(col) <- levs
	}

	# === plot boxes ===
	# trick from http://stackoverflow.com/questions/15627674/

	cuts <- function(x) {
	  n <- length(x) %/% 4
	  map <- rep(c(rep(TRUE,4),FALSE), n)
	  result <- rep(NA, n*5)
	  result[map] <- x
	  result
	}

	for (i in levs) {

    # undocumented special coloring
    # should be removed at some point
		if (i == "-"){
			col[i] = "lightgrey"
		}

		todo <- which(x == i, arr.ind=T)
		todoX <- cbind(todo[,1]-1, todo[,1]-1, todo[,1], todo[,1])
		todoY <- cbind(todo[,2]-1, todo[,2], todo[,2], todo[,2]-1)
		polygon(  cuts(t(todoX))
				, cuts(t(todoY))
				, col = col[i]
				, border = NA
				)
	}

  # === add boxes for internal grouping ===
  # coloring is difficul to get right, option ignored for now

  if (FALSE) {

  col.sim <- function(sim, order, levels = 30) {
    dist <- as.matrix(max(sim)-sim)
    reorder <- dist[order,order]
    cut <- reorder[-nrow(sim),-1]
    d <- diag(cut)
    norm <- ceiling( (d/max(d))^4 * 9 + 1 )
    return(rev(heat.colors(10))[norm])
  }

  rx <- nrow(x)-1
  cx <- ncol(x)-1

  row.boxesX <- cbind( (1:rx)-0.5, (1:rx)-0.5, (1:rx)+0.5, (1:rx)+0.5 )
  row.boxesY <- cbind( rep(-1,rx), rep(-2,rx), rep(-2,rx), rep(-1,rx) )

  polygon (cuts(t(row.boxesX))
           , cuts(t(row.boxesY))
           , col = col.sim(sim.rows, order.rows)
           , border = NA
           )

  col.boxesY <- cbind( (1:cx)-0.5, (1:cx)-0.5, (1:cx)+0.5, (1:cx)+0.5 )
  col.boxesX <- cbind( rep(-1,cx), rep(-2,cx), rep(-2,cx), rep(-1,cx) )

  polygon (cuts(t(col.boxesX))
           , cuts(t(col.boxesY))
           , col = col.sim(sim.cols, order.cols)
           , border = NA
  )

  }

	# === add names of rare levels ===

	if (show.remaining) {
		remaining <- all[is.na(match(all,levs))]
		for (i in remaining) {
			todo <- which(x == i, arr.ind = T)
			text(todo - 0.5
				, labels = i
				, cex = cex.remaining
				, col = "grey"
				)
		}
		col.remaining = "grey"
		pch.remaining = 4
	} else {
		col.remaining = "white"
		pch.remaining = 0
	}

	# === add legend ===

	legend(x = dim(x)[1]
		 , y = dim(x)[2]
		 , legend = c(levs, "other", "NA")
		 , xpd = TRUE
		 , pch = c(rep(15, times =  length(levs)), pch.remaining, 20)
		 , col = c(col, col.remaining, "grey")
		 , bty = "n"
		 , cex = cex.legend
		 , ncol = 1
		 )

	# === return to default par settings ===

	par(op)

}
