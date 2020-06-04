# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3

if (!isGeneric("getValues")) {
	setGeneric("getValues", function(x, row, nrows, ...)
		standardGeneric("getValues"))
}	

setMethod("getValues", signature(x='RasterLayer', row='missing', nrows='missing'), 
function(x, format='') {
	
	cr <- c(x@ncols, x@nrows)
#	f <- is.factor(x)
#	if (f) {
#		labs <- labels(x)
#	}
	
	if ( inMemory(x) ) {
		x <- x@data@values
	} else if ( fromDisk(x) ) {
		x <- .readRasterLayerValues(x, 1, x@nrows)
	} else {
		x <- rep(NA, ncell(x))
	}

	if (format=='matrix') { 
		return ( matrix(x, ncol=cr[1], nrow=cr[2], byrow=TRUE) )
	#} else if (format =='array') {
	#	return( array( matrix(x, ncol=cr[1], nrow=cr[2], byrow=TRUE), dim=c(cr, 1)) )
		
#	} else if (f) {
#		x <- factor(x)
		# set labels?
	}

	return( x ) 
}
)


setMethod("getValues", signature(x='RasterBrick', row='missing', nrows='missing'), 
function(x) {
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			x <- readAll(x)
		} else {
			return( matrix(rep(NA, ncell(x) * nlayers(x)), ncol=nlayers(x)) )
		}
	}
	colnames(x@data@values) <- names(x)
	x@data@values
}
)



setMethod("getValues", signature(x='RasterStack', row='missing', nrows='missing'), 
function(x) {
	m <- matrix(nrow=ncell(x), ncol=nlayers(x))
	colnames(m) <- names(x)
	for (i in 1:nlayers(x)) {
		m[,i] <- getValues(x@layers[[i]])
	}
	m
}
)

