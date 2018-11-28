# Author: Robert J. Hijmans
# Date :  September 2011
# Version 1.0
# Licence GPL v3



.adjacentUD <- function(x, cells, ngb, include) {
	# ngb should be a matrix with 
	# one and only one cell with value 0 (the focal cell), 
	# at least one cell with value 1 (the adjacent cells)
	# cells with other values are ignored (not considered adjacent)
	rs <- res(x)

	rn <- raster(ngb)
	center <- which(values(rn)==0)
	if (include) {
		ngb[center] <- 1
	}
	rc <- rowFromCell(rn, center)
	cc <- colFromCell(rn, center)
	
	xngb <- yngb <- ngb
	xngb[] <- rep(1:ncol(ngb), each=nrow(ngb)) - cc 
	yngb[] <- rep(nrow(ngb):1, ncol(ngb)) - (nrow(ngb)-rc+1)
	ngb[ngb != 1] <- NA
	xngb <- stats::na.omit(as.vector( xngb * rs[1] * ngb))
	yngb <- stats::na.omit(as.vector( yngb * rs[2] * ngb))
		
	xy <- xyFromCell(x, cells)
	X <- apply(xy[,1,drop=FALSE], 1, function(z) z + xngb )
	Y <- apply(xy[,2,drop=FALSE], 1, function(z) z + yngb )

	c(as.vector(X), as.vector(Y))
}


setMethod("adjacent", signature(x="RasterLayer"), 
function(x, cells, directions=4, pairs=TRUE, target=NULL, sorted=FALSE, include=FALSE, id=FALSE) {

	if (is.character(directions)) { 
		directions <- tolower(directions) 
	}

	x <- raster(x)
	r <- res(x)
	xy <- xyFromCell(x, cells)

	mat <- FALSE
	if (is.matrix(directions)) {
		stopifnot(length(which(directions==0)) == 1)
		stopifnot(length(which(directions==1)) > 0)
		
		d <- .adjacentUD(x, cells, directions, include)
		
		directions <- sum(directions==1, na.rm=TRUE)
		mat <- TRUE
		
	} else if (directions==4) {
		if (include) {
			d <- c(xy[,1], xy[,1]-r[1], xy[,1]+r[1], xy[,1], xy[,1], xy[,2], xy[,2], xy[,2], xy[,2]+r[2], xy[,2]-r[2])
		} else {
			d <- c(xy[,1]-r[1], xy[,1]+r[1], xy[,1], xy[,1], xy[,2], xy[,2], xy[,2]+r[2], xy[,2]-r[2])
		}
		
	} else if (directions==8) {
		if (include) {
			d <- c(xy[,1], rep(xy[,1]-r[1], 3), rep(xy[,1]+r[1],3), xy[,1], xy[,1],
				 xy[,2], rep(c(xy[,2]+r[2], xy[,2], xy[,2]-r[2]), 2), xy[,2]+r[2], xy[,2]-r[2])
		} else {
			d <- c(rep(xy[,1]-r[1], 3), rep(xy[,1]+r[1],3), xy[,1], xy[,1],
				rep(c(xy[,2]+r[2], xy[,2], xy[,2]-r[2]), 2), xy[,2]+r[2], xy[,2]-r[2])
		}
	} else if (directions==16) {
		r2 <- r * 2
		if (include) {
			d <- c(xy[,1], rep(xy[,1]-r2[1], 2), rep(xy[,1]+r2[1], 2),
				rep(xy[,1]-r[1], 5), rep(xy[,1]+r[1], 5),
				xy[,1], xy[,1], 
							
				xy[,2], rep(c(xy[,2]+r[2], xy[,2]-r[2]), 2),
				rep(c(xy[,2]+r2[2], xy[,2]+r[2], xy[,2], xy[,2]-r[2], xy[,2]-r2[2]), 2),
				xy[,2]+r[2], xy[,2]-r[2])

		} else {
			d <- c(rep(xy[,1]-r2[1], 2), rep(xy[,1]+r2[1], 2),
				rep(xy[,1]-r[1], 5), rep(xy[,1]+r[1], 5),
				xy[,1], xy[,1], 
							
				rep(c(xy[,2]+r[2], xy[,2]-r[2]), 2),
				rep(c(xy[,2]+r2[2], xy[,2]+r[2], xy[,2], xy[,2]-r[2], xy[,2]-r2[2]), 2),
				xy[,2]+r[2], xy[,2]-r[2])
		}					
							
	} else if (directions=='bishop') {
		if (include) {
			d <- c(xy[,1], rep(xy[,1]-r[1], 2), rep(xy[,1]+r[1],2), xy[,2], rep(c(xy[,2]+r[2], xy[,2]-r[2]), 2))
		} else {
			d <- c(rep(xy[,1]-r[1], 2), rep(xy[,1]+r[1],2), rep(c(xy[,2]+r[2], xy[,2]-r[2]), 2))		
		}
		directions <- 4 # to make pairs
		
	} else {
		stop('directions should be one of: 4, 8, 16, "bishop", or a matrix')
	}

	if (include) directions <- directions + 1
	
	d <- matrix(d, ncol=2)
	if (.isGlobalLonLat(x)) {
		# normalize longitude to -180..180
		d[,1] <- (d[,1] + 180) %% 360 - 180
	}
	
	if (pairs) {
		if (mat) {
			cell <- rep(cells, each=directions)		
		} else {
			cell <- rep(cells, directions)
		}
		
		if (id) {
			if (mat) {
				ID <- rep(1:length(cells), each=directions)
			} else {
				ID <- rep(1:length(cells), directions)
			}

			d <- stats::na.omit(cbind(ID, cell, cellFromXY(x, d)))
			attr(d, 'na.action') <- NULL
			colnames(d) <- c('id', 'from', 'to')
			if (! is.null(target)) {
				d <- d[d[,3] %in% target, ]
			}
			
		} else {
			d <- stats::na.omit(cbind(cell, cellFromXY(x, d)))
			attr(d, 'na.action') <- NULL
			colnames(d) <- c('from', 'to')
			if (! is.null(target)) {
				d <- d[d[,2] %in% target, ]
			}
		}
		if (sorted) {
			d <- d[order(d[,1], d[,2]),]
		}
	} else {
		d <- as.vector(unique(stats::na.omit(cellFromXY(x, d))))
		if (! is.null(target)) {
			d <- intersect(d, target)
		}
		if (sorted) {
			d <- sort(d)
		}
	}
	d
}
)


