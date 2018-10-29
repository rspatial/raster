# Author: Robert J. Hijmans
# Date : November 2010
# Version 1.0
# Licence GPL v3


alignExtent <- function(extent, object, snap='near') {

	snap <- tolower(snap)
	stopifnot(snap %in% c('near', 'in', 'out'))
	
	extent <- extent(extent)
	if (!inherits(object, 'BasicRaster')) { stop('object should inherit from BasicRaster') }
	res <- res(object)
	orig <- origin(object)
	
# snap points to pixel boundaries

	if (snap == 'near') {
		xmn <- round((extent@xmin-orig[1]) / res[1]) * res[1] + orig[1]
		xmx <- round((extent@xmax-orig[1]) / res[1]) * res[1] + orig[1]
		ymn <- round((extent@ymin-orig[2]) / res[2]) * res[2] + orig[2]
		ymx <- round((extent@ymax-orig[2]) / res[2]) * res[2] + orig[2]
	} else if (snap == 'out') {
		xmn <- floor((extent@xmin-orig[1]) / res[1]) * res[1] + orig[1]
		xmx <- ceiling((extent@xmax-orig[1]) / res[1]) * res[1] + orig[1]
		ymn <- floor((extent@ymin-orig[2]) / res[2]) * res[2] + orig[2]
		ymx <- ceiling((extent@ymax-orig[2]) / res[2]) * res[2] + orig[2]
	} else if (snap == 'in') {
		xmn <- ceiling((extent@xmin-orig[1]) / res[1]) * res[1] + orig[1]
		xmx <- floor((extent@xmax-orig[1]) / res[1]) * res[1] + orig[1]
		ymn <- ceiling((extent@ymin-orig[2]) / res[2]) * res[2] + orig[2]
		ymx <- floor((extent@ymax-orig[2]) / res[2]) * res[2] + orig[2]
	}
	
	if (xmn == xmx) {
		if (xmn <= extent@xmin) {
			xmx <- xmx + res[1]
		} else {
			xmn <- xmn - res[1]		
		}
	}
	if (ymn == ymx) {
		if (ymn <= extent@ymin) {
			ymx <- ymx + res[2]
		} else {
			ymn <- ymn - res[2]		
		}
	}
	e <- extent(xmn, xmx, ymn, ymx)
	intersect(e, extent(object))
}


.Old.alignExtent <- function(extent, object) {
	object <- raster(object)
	oldext <- extent(object)
	e <- extent(extent)
	e@xmin <- min(e@xmin, oldext@xmin)
	e@xmax <- max(e@xmax, oldext@xmax)
	e@ymin <- min(e@ymin, oldext@ymin)
	e@ymax <- max(e@ymax, oldext@ymax)
	
	
	col <- colFromX(object, e@xmin)
	mn <- xFromCol(object, col) - 0.5 * xres(object)
	mx <- xFromCol(object, col) + 0.5 * xres(object)
	if (abs(e@xmin - mn) > abs(e@xmin - mx)) { 
		e@xmin <- mx 
	} else { 
		e@xmin <- mn 
	}
	col <- colFromX(object, e@xmax)
	if (is.na(col))
	mn <- xFromCol(object, col) - 0.5 * xres(object)
	mx <- xFromCol(object, col) + 0.5 * xres(object)
	if (abs(e@xmax - mn) > abs(e@xmax - mx)) { 
		e@xmax <- mx 
	} else { 
		e@xmax <- mn 
	}
	
	row <- rowFromY(object, e@ymin)
	mn <- yFromRow(object, row) - 0.5 * yres(object)
	mx <- yFromRow(object, row) + 0.5 * yres(object)
	if (abs(e@ymin - mn) > abs(e@ymin - mx)) {
		e@ymin <- mx
	} else { 
		e@ymin <- mn 
	}
	row <- rowFromY(object, e@ymax)
	mn <- yFromRow(object, row) - 0.5 * yres(object)
	mx <- yFromRow(object, row) + 0.5 * yres(object)
	if (abs(e@ymax - mn) > abs(e@ymax - mx)) { 
		e@ymax <- mx 
	} else {
		e@ymax <- mn 
	}
	
	if ( e@ymin == e@ymax ) {
		if (oldext@ymax > e@ymax) {
			e@ymax = e@ymax + yres(object)
		} 
		if (oldext@ymin < e@ymin) {
			e@ymin = e@ymin - yres(object)		
		}
	}
	if ( e@xmin == e@xmax ) {
		if (oldext@xmax > e@xmax) {
			e@xmax = e@xmax + xres(object)
		} 
		if (oldext@xmin < e@xmin) {
			e@xmin = e@xmin - xres(object)		
		}
	}
	return(e)
}


