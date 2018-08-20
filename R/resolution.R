# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


'res<-' <- function(x, value) {
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
}

