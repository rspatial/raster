# Author: Robert J. Hijmans  and Jacob van Etten
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.pointsToMatrix <- function(p) {
	if (inherits(p, 'SpatialPoints')) {
		p <- coordinates(p)
	} else if (is.data.frame(p)) {
		p <- as.matrix(p)
	} else if (is.vector(p)){
		if (length(p) != 2) {
			stop('Wrong length for a vector, should be 2')
		} else {
			p <- matrix(p, ncol=2) 
		}
	}
	if (is.matrix(p)) {
		if (ncol(p) != 2) {
			stop( 'A points matrix should have 2 columns')
		}
		cn <- colnames(p)
		if (length(cn) == 2) {
			if (toupper(cn[1]) == 'Y' | toupper(cn[2]) == 'X')  {
				stop('Highly suspect column names (x and y reversed?)')
			}
			if (toupper(substr(cn[1],1,3) == 'LAT' | toupper(substr(cn[2],1,3)) == 'LON'))  {
				stop('Highly suspect column names (longitude and latitude reversed?)')
			}
		}		
	} else {
		stop('points should be vectors of length 2, matrices with 2 columns, or a SpatialPoints* object')
	}

	return(p)
}


.distm <- function (x, longlat) {
	if (longlat) { 
		n <- nrow(x)
		dm <- matrix(ncol = n, nrow = n)
		dm[cbind(1:n, 1:n)] <- 0
		if (n > 1) {
			for (i in 2:n) {
				j = 1:(i - 1)
				dm[i, j] = .geodist(x[i, 1], x[i, 2], x[j, 1], x[j, 2])
			}
		}
		return(dm)
	} else { 
		return(.planedist2(x, x))
	}
}


.distm2 <- function (x, y, longlat) {
	if (longlat) { 
		n <- nrow(x)
		m <- nrow(y)
		dm <- matrix(ncol=m, nrow=n)
		for (i in 1:n) {
			dm[i,] <- .geodist(x[i, 1], x[i, 2], y[, 1], y[, 2])
		}
		return(dm)
	} else { 
		return(.planedist2(x, y))
		# fun <- .planedist
	}
}

.distm2new <- function (x, y, longlat, a=6378137, f=1/298.257223563) {
	if (longlat) { 
		n <- nrow(x)
		m <- nrow(y)
		
		xx <- cbind(rep(x[,1], m), rep(x[,2], m))
		yy <- cbind(rep(y[,1], each=n), rep(y[,2], each=n))
		
		g <- .Call("_raster_point_distance", xx, yy, TRUE, a, f, PACKAGE='raster')
		return(matrix(g, n, m))
	} else { 
		return(.planedist2(x, y))
		# fun <- .planedist
	}
}


pointDistance <- function (p1, p2, lonlat, allpairs=FALSE, ...) {
	
	longlat <- list(...)$longlat
	if (!is.null(longlat)) {
		lonlat <- longlat
	}

	if (missing(lonlat)) {
		if (isLonLat(p1)) {
			lonlat <- TRUE
		} else if (! is.na(projection(p1)) ) {
			lonlat <- FALSE		
		} else {
			stop('you must provide a "lonlat" argument (TRUE/FALSE)')
		}
	}
	stopifnot(is.logical(lonlat)) 
	
	p1 <- .pointsToMatrix(p1)
	if (missing(p2)) {
		return(.distm(p1, lonlat))
	}
	
	p2 <- .pointsToMatrix(p2)
	
	if (nrow(p1) != nrow(p2)) {
		allpairs <- TRUE
	}
	
	if (allpairs) {
		if(nrow(p1) > 1 & nrow(p2) > 1) {
			return(.distm2(p1, p2, lonlat))
		}
	}
	
	if (lonlat ) {
#		return( .haversine(p1[,1], p1[,2], p2[,1], p2[,2], r=6378137) )
		return( .geodist(p1[,1], p1[,2], p2[,1], p2[,2]) )
	} else { 
		return( .planedist(p1[,1], p1[,2], p2[,1], p2[,2]) )
	}
}

.planedist <- function(x1, y1, x2, y2) {
	sqrt(( x1 -  x2)^2 + (y1 - y2)^2) 
}


.planedist2 <- function(p1, p2) {
# code by Bill Venables
# https://stat.ethz.ch/pipermail/r-help/2008-February/153841.html
	z0 <- complex(, p1[,1], p1[,2])
	z1 <- complex(, p2[,1], p2[,2])
	outer(z0, z1, function(z0, z1) Mod(z0-z1))
}


.geodist <- function(x1, y1, x2, y2, a=6378137, f=1/298.257223563) {
	# recycle
    p <- cbind(x1, y1, x2, y2)
	.Call("_raster_point_distance", p[,1:2, drop=FALSE], p[, 3:4,drop=FALSE], TRUE, a, f, PACKAGE='raster')
#	.Call("inversegeodesic", as.double(p[,1]), as.double(p[,2]), as.double(p[,3]), as.double(p[,4]), as.double(a), as.double(f), PACKAGE='raster')
}


.old_haversine <- function(x1, y1, x2, y2, r=6378137) {
	adj <- pi / 180
	x1 <- x1 * adj
	y1 <- y1 * adj
	x2 <- x2 * adj
	y2 <- y2 * adj
	x <- sqrt((cos(y2) * sin(x1-x2))^2 + (cos(y1) * sin(y2) - sin(y1) * cos(y2) * cos(x1-x2))^2)
	y <- sin(y1) * sin(y2) + cos(y1) * cos(y2) * cos(x1-x2)
	return ( r * atan2(x, y) )
}

