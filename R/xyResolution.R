# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3

setMethod('xres', signature(x='BasicRaster'), 
function(x) {
	if (rotated(x)) {
		return(x@rotation@geotrans[3])
	} else {
		e <- x@extent
		return ( (e@xmax - e@xmin) / x@ncols )  
	}
} )

setMethod('yres', signature(x='BasicRaster'), 
function(x) {
	if (rotated(x)) {
		return(x@rotation@geotrans[5])
	} else {
		e <- x@extent
		return ( (e@ymax - e@ymin) / x@nrows )  
	}
} )

setMethod('res', signature(x='BasicRaster'), 
function(x) {
	if (rotated(x)) {
		return(x@rotation@geotrans[c(2,6)])
	} else {
		e <- x@extent
		xr <- (e@xmax - e@xmin) / x@ncols 
		yr <- (e@ymax - e@ymin) / x@nrows
		return( c(xr, yr) )
	}
} )




setMethod('res<-', signature(x='BasicRaster'), 
function(x, value) {

	if (rotated(x)) {
		stop('cannot set the resolution of a rotated raster')
	}

	if (length(value) == 1) {
		xr=value
		yr=value
	} else {
		xr=value[1]
		yr=value[2]
	}
	
	bb <- extent(x)
	nc <- max(1, round( (bb@xmax - bb@xmin) / xr ))
	nr <- max(1, round( (bb@ymax - bb@ymin) / yr ))
	if (nr != x@nrows | nc != x@ncols) {
		if (methods::extends(class(x), "Raster")) {
			x <- clearValues(x)
		}
	}
	bb@xmax <- bb@xmin + nc * xr
	bb@ymin <- bb@ymax - nr * yr
	extent(x) <- bb
	dim(x) <- c(nr, nc)
	return(x)
} )




