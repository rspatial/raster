# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3

	


setMethod('intersect', signature(x='Raster', y='ANY'), 
function(x, y) {
	y <- extent(y)
	crop(x, y)
} )



setMethod('intersect', signature(x='Extent', y='ANY'), 
function(x, y) {

	y <- extent(y)
	
	x@xmin <- max(x@xmin, y@xmin)
	x@xmax <- min(x@xmax, y@xmax)
	x@ymin <- max(x@ymin, y@ymin)
	x@ymax <- min(x@ymax, y@ymax)

	if ((x@xmax <= x@xmin) | (x@ymax <= x@ymin) ) {
		#warning('Objects do not overlap')
		return(NULL)
	}
	return(x)
} )




.intersectExtent <- function(x, ..., validate=TRUE) {
	objects <- c(x, list(...))
	if (length(objects) == 1) {
		return(extent(x))
	}
	e <- extent(objects[[1]])
	for (i in 2:length(objects)) {
		e2 <- extent(objects[[i]])
		e@xmin <- max(e@xmin, e2@xmin)
		e@xmax <- min(e@xmax, e2@xmax)
		e@ymin <- max(e@ymin, e2@ymin)
		e@ymax <- min(e@ymax, e2@ymax)
	}
	if ((e@xmax <= e@xmin) | (e@ymax <= e@ymin) ) {
		if (validate) {
			stop('Objects do not intersect')
		} else {
			return(NULL)
		}
	}
	return(e)
}


