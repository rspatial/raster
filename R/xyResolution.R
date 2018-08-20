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

