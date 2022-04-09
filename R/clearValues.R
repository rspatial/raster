# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.clearRaster <- function(object) {
	object@data@inmemory <- FALSE
	
	
#	object@data@indices = vector(mode='numeric')
	object@data@values <- vector()
	if ( !  fromDisk(object) ) {
		object@data@min <- Inf
		object@data@max <- -Inf	
		object@data@haveminmax <- FALSE
	}	
	return(object)
}


clearValues <- function(x) {
	if (inherits(x, "BasicRaster")) {
		return(x)
	} else if (inherits(x, "RasterLayer" )) {
		x <- .clearRaster(x)
	} else if (inherits(x, "RasterStack") ) {
		for (i in seq(along.with=nlayers(x))) {
			if (fromDisk(x@layers[[i]])) {
				x@layers[[i]] <- .clearRaster(x@layers[[i]])
			}
		}
	} else if (inherits(x, 'RasterBrick')) {
		x@data@values <- matrix(NA,0,0)
		x@data@inmemory <- FALSE
		
#		x@data@indices = c(0,0)
		if ( !  fromDisk(x) ) {
			x@data@min <- rep(Inf, nlayers(x))
			x@data@max <- rep(-Inf, nlayers(x))
			x@data@haveminmax <- FALSE
		}
	} 
	return(x)
}


.clearFile <- function(x) {
	x@file@name <- ''
	x@data@fromdisk <- FALSE
	x@file@driver <- ""
	return(x)
}
