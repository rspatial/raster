# Author: Robert J. Hijmans
# Date :  July 2010
# Version 1.0
# Licence GPL v3


rasterFromXYZ <- function(xyz, res=c(NA, NA), crs="", digits=5) {

	if (length(res) == 1) res = c(res, res)

	if (inherits(xyz, 'SpatialPoints')) {
		if (inherits(xyz, 'SpatialPointsDataFrame')) {
			xyz <- cbind(coordinates(xyz)[,1:2,drop=FALSE], xyz@data[,1])
		} else {
			xyz <- coordinates(xyz)[,1:2,drop=FALSE]		
		}
	}
	
	ln <- colnames(xyz)
	
	if (inherits(xyz, 'data.frame')) {
		xyz <- as.matrix(xyz)
		xyz <- matrix(as.numeric(xyz), ncol=ncol(xyz), nrow=nrow(xyz))
	}
	xyz <- xyz[(!is.na(xyz[,1])) & (!is.na(xyz[,2])), ]
	
	x <- sort(unique(xyz[,1]))
	dx <- x[-1] - x[-length(x)]

	if (is.na(res[1])) {
		if (length(x) < 2) {
			stop("more than one unique x value needed")
		}
		rx <- min(dx)
		for (i in 1:5) {
			rx <- rx / i
			q <- sum(round(dx / rx, digits=digits) %% 1)
			if ( q == 0 ) {
				break
			}
		}
		if ( q > 0 ) {
			stop('x cell sizes are not regular')
		}
	} else {
		rx <- res[1]
		test <- sum(round(dx / rx, digits=digits) %% 1)
		if ( test > 0 ) {
			stop('x cell sizes are not regular')
		}
	}
	
	y <- sort(unique(xyz[,2]))
	dy <- y[-1] - y[-length(y)]
	# probably a mistake to use the line below 
	# Gareth Davies suggested that it be removed 
	# dy <- round(dy, digits)
	
	if (is.na(res[2])) {
		if (length(y) < 2) {
			stop("more than one unique y value needed")
		}
		ry <- min(dy)
		for (i in 1:5) {
			ry <- ry / i
			q <- sum(round(dy / ry, digits=digits) %% 1)
			if ( q == 0 ) {
				break
			}
		}
		if ( q > 0 ) {
			stop('y cell sizes are not regular')
		}
	} else {
		ry <- res[2]
		test <- sum(round(dy / ry, digits=digits) %% 1)
		if ( test > 0 ) {
			stop('y cell sizes are not regular')
		}
	}
	
	minx <- min(x) - 0.5 * rx
	maxx <- max(x) + 0.5 * rx
	miny <- min(y) - 0.5 * ry
	maxy <- max(y) + 0.5 * ry
	
	d <- dim(xyz)
	if (d[2] <= 3) {
		r <- raster(xmn=minx, xmx=maxx, ymn=miny, ymx=maxy, crs=crs)
	} else {
		r <- brick(xmn=minx, xmx=maxx, ymn=miny, ymx=maxy, crs=crs, nl=d[2]-2)	
	}

	res(r) <- c(rx, ry)
	cells <- cellFromXY(r, xyz[,1:2])
	if (d[2] > 2) {
		names(r) <- ln[-c(1:2)]
		r[cells] <- xyz[,3:d[2]]
	} 	
	return(r)
}	
	

