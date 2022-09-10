# Author: Robert J. Hijmans
# Date : September 2010
# Version 1.0
# Licence GPL v3


'gain<-' <- function(x, value) {
	value <- as.numeric(value[1])
	if (inherits(x, 'RasterStack')) {
		x@layers <- lapply( x@layers, 
			function(z) {
				if (fromDisk(z)) {
					z@data@gain <- value
				} else if (hasValues(z)) {
					z <- z * value
				}
				return(z)
			} 
		)
	} else {
		if (fromDisk(x)) {
			x@data@gain <- value
		} else if (hasValues(x)) {
			x <- x * value
		}
	}
	return(x)
}


gain <- function(x) {
	if (inherits(x, 'RasterStack')) {
		r <- sapply( x@layers, function(z) { z@data@gain } )
	} else {
		r <- x@data@gain 		
	}
	return(r)
}


'offs<-' <- function(x, value) {
	value <- as.numeric(value[1])
	if (inherits(x, 'RasterStack')) {
	
		x@layers <- lapply( x@layers, 
			function(z) { 
				if (fromDisk(z)) {
					z@data@offset <- value
				} else if (hasValues(z)) {				
					z <- z + value
				}
				return(z) 
			} 
		)
			
	} else {
		if (fromDisk(x)) {
			x@data@offset <- value
		} else if (hasValues(x)) {
			x <- x + value
		}
	}
	return(x)
}


offs <- function(x) {
	if (inherits(x, 'RasterStack')) {
		r <- sapply( x@layers, function(z) { z@data@offset } )
	} else {
		r <- x@data@offset 
	}
	return(r)
}

