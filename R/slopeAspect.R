# Author: Robert J. Hijmans
# Date : February 2011
# Version 1.0
# Licence GPL v3


slopeAspect <- function(dem, filename='', out=c('slope', 'aspect'), unit='radians', neighbors=8, flatAspect, ...) {
	
	warning('this function is deprecated. Please use function "terrain" instead')
	
	stopifnot(neighbors %in% c(4, 8))
	stopifnot(! is.na(projection(dem)) )
	unit <- trim(tolower(unit))
	stopifnot(unit %in% c('degrees', 'radians'))
	filename <- trim(filename)

	out <- trim(tolower(out))
	stopifnot(all(out %in% c('slope', 'aspect')))
	
	
	if (length(out) == 1) {
		type <- out
	} else {
		type <- 'both'
	}
	
	
	res <- res(dem)
	dx <- res[1]
	dy <- res[2]
	if (neighbors == 8) {
		fX <- matrix(c(-1,-2,-1,0,0,0,1,2,1) / -8, nrow=3)
		fY <- matrix(c(-1,0,1,-2,0,2,-1,0,1) / 8, nrow=3)
	} else { # neighbors == 4
		fX <- matrix(c(0,-1,0,0,0,0,0,1,0) / -2, nrow=3)
		fY <- matrix(c(0,0,0,-1,0,1,0,0,0) / 2, nrow=3)
	}
	
	lonlat <- isLonLat(dem)
	if (!lonlat & couldBeLonLat(dem)) {
		warning('assuming crs is longitude/latitude')
		lonlat <- TRUE
	}
	
	if (lonlat) {
		dy <- pointDistance(cbind(0,0), cbind(0, dy), lonlat=TRUE)
		fY <- fY / dy
		
		zy <- focal(dem, w=fY)
		zx <- focal(dem, w=fX)
		
		y <- yFromRow(dem, 1:nrow(dem))
		dx <- .geodist(-dx, y, dx, y) / 2
		zx <- t( t(zx) / dx)
		
	} else {
	
		fX <- fX / dx
		fY <- fY / dy
		zx <- focal(dem, w=fX)
		zy <- focal(dem, w=fY)
	}

	if (type == 'slope') {
		
		x <- atan( sqrt( zy^2 + zx^2 ) )
		if (unit == 'degrees') {
			x <- x * (180 / pi)
		}
		names(x) <- 'slope'
		
	} else if (type == 'aspect') {
		x <- atan2(zy, zx)
		x <- ((0.5*pi)-x) %% (2*pi)
		if (unit == 'degrees') {
			x <- x * (180/pi)
		}
		if (!missing (flatAspect)) {
			slope <-  sqrt( zy^2 + zx^2 ) 
			aspect <- overlay(x, slope, fun=function(x, y) { x[y==0] <- flatAspect; return(x) } )
		}
		names(x) <- 'aspect'
		
	} else {
		x <- atan( sqrt( zy^2 + zx^2 ) )
		aspect <- atan2(zy, zx) 
		aspect <- ((0.5*pi)-aspect) %% (2*pi)
		
		if (unit == 'degrees') {
			x <- x * (180/pi)
			aspect <- aspect * (180/pi)
		}
		if (!missing (flatAspect)) {
			aspect <- overlay(aspect, x, fun=function(x, y) { x[y==0] <- flatAspect; return(x) } )
		}
		
		names(x) <- 'slope'
		names(aspect) <- 'aspect'
		x <- stack(x, aspect)
	}

	if (filename != "") {
		x <- writeRaster(x, filename, ...)
	}
	return(x)
}

