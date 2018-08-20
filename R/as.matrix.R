# Author: Robert J. Hijmans 
# Date : October 2010
# Version 1.0
# Licence GPL v3


setMethod('as.matrix', signature(x='RasterLayer'), 
function(x, maxpixels, ...) {
	if (!hasValues(x)) { stop("'x' has no values") }
	if (! missing(maxpixels)) {
		x <- sampleRegular(x, maxpixels, asRaster=TRUE)
	}
	return( getValues(x, format='matrix') )
})


setMethod('as.matrix', signature(x='RasterStackBrick'), 
function(x, maxpixels, ...){ 
	if (!hasValues(x)) { stop("'x' has no values") }
	if (! missing(maxpixels)) {
		x <- sampleRegular(x, maxpixels, asRaster=TRUE)
	}
	return( getValues(x) )
})


setMethod('as.matrix', signature(x='Extent'), 
function(x, ...) {
	b <- bbox(x)
	rownames(b) <- c('x', 'y')
	b
})


# mode argument is ignored as mode=mode gave an error on R-devel
setMethod('as.vector', signature(x='Extent'), 
function(x, mode='any') {
	as.vector(c(x@xmin, x@xmax, x@ymin, x@ymax))
})


setMethod('as.vector', signature(x='Raster'), 
function(x, mode='any') {
	as.vector(getValues(x))
})

