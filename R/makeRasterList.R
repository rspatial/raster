# Author: Robert J. Hijmans
# Date : September 2008
# Version 0.9
# Licence GPL v3

.addToList <- function(x, r, compare, giveError, unstack) {
	if (class(r) == 'character') {
		r <- raster(r)
		# or r <- unstack(stack(r, -1)) ???
		if (compare & length(x)>0) { 
			compareRaster(x[[1]], r)  
		}
		return( c(x, r) )
	} else if (! methods::extends(class(r), 'Raster')) {
		if (giveError) {
			stop('... arguments must be a filename or objects that extend the Raster class')
		} else {
			return(x)
		}
	} else if (unstack & inherits(r, 'RasterStackBrick')) { 
		if ( compare & length(x) > 0 ) { 
			compareRaster(x[[1]], r)  
		}
		return( c(x, unstack(r)) )
	} else {
		if (compare & length(x) > 0) { 
			compareRaster(x[[1]], r)  
		}
		return( c(x, r) )	
	} 
}



.makeRasterList <- function(..., compare=FALSE, giveError=FALSE, unstack=TRUE) {
	arg <- list(...)
	x <- list()
	for (i in seq(along.with=arg)) {
		if (class(arg[[i]]) == 'list') {
			for (j in seq(along.with=arg[[i]])) {
				x <- .addToList(x, arg[[i]][[j]], compare=compare, giveError=giveError, unstack=unstack) 
			}
		} else {
			x <- .addToList(x, arg[[i]], compare=compare, giveError=giveError, unstack=unstack) 
		}
	}

	fdim <- sapply(x, fromDisk) & sapply(x, inMemory)
	if (sum(fdim) > 0) {
		x[fdim] <- sapply(x[fdim], clearValues)
	}
	hv <- sapply(x, hasValues)
	if (sum(hv) < length(x)) {
		if (sum(hv) == 0) {
			x <- x[1]
		} else {
			x <- x[hv]
			warning('layer(s) with no data ignored')
		}
	}
	return(x)
}


setMethod('as.list', signature(x='Raster'), 
function(x, ...) {
	.makeRasterList(x, ...)
}
)

