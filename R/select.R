# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("select")) {
	setGeneric("select", function(x, ...)
		standardGeneric("select"))
}


setMethod('select', signature(x='Raster'), 
	function(x, use='rec', ...) {
		use <- substr(tolower(use), 1, 3)
		stopifnot(use %in% c('rec', 'pol'))
		if (use == 'rec') {
			e <- drawExtent()
			int <- intersect(e, extent(x))
			if (is.null(int)) {
				x <- NULL
			} else {
				x <- crop(x, e)
			}
		} else {
			e <- drawPoly()
			int <- intersect(extent(x), e)
			if (is.null(int)) {
				x <- NULL
			} else {
				x <- crop(x, e)
				x <- mask(x, e)
			}
		}
		x
	}
)
	
	
setMethod('select', signature(x='Spatial'), 
	function(x, use='rec', draw=TRUE, col='cyan', size=2, ...) {
		use <- substr(tolower(use), 1, 3)
		stopifnot(use %in% c('rec', 'pol'))
		if (use == 'rec') {
			e <- as(drawExtent(), 'SpatialPolygons')
		} else {
			e <- drawPoly()
		}
		e@proj4string <- x@proj4string
		int <- intersect(extent(e), extent(x))
		if (is.null(int)) {
			return(  NULL )
		}

		if (inherits(x, 'SpatialPolygons')) {
			on.exit(rgeos::set_RGEOS_CheckValidity(.checkGEOS()))
		
			int <- rgeos::gIntersects(x, e, byid=TRUE)
			int <- apply(int, 2, any)
			if (any(int)) {
				x <- x[int, ]
				if (draw) {
					sp::plot(x, add=TRUE, border=col, lwd=size)
				}
			} else {
				x <- NULL
			}
			
		} else if (inherits(x, 'SpatialLines')) {
			on.exit(rgeos::set_RGEOS_CheckValidity(.checkGEOS()))
			
			int <- rgeos::gIntersects(x, e, byid=TRUE)
			int <- apply(int, 2, any)
			if (any(int)) {
				x <- x[int, ]
				if (draw) {
					sp::plot(x, add=TRUE, col=col, lwd=size)
				}
			} else {
				x <- NULL
			}
			
		} else if (inherits(x, 'SpatialGrid')) {
			cls <- class(x)
			if (.hasSlot(x, 'data')) {
				x <- as(x, 'SpatialPointsDataFrame')
			} else {
				x <- as(x, 'SpatialPoints')			
			}
			i <- which(!is.na(over(x, e)))
			if (length(i) > 0) {
				x <- x[i,]
				gridded(x) <- TRUE
				x <- as(x, cls)
				if (draw) {
					sp::plot(x, col=col, cex=size, add=TRUE)
				}
			} else {
				x <- NULL
			}

		} else if (inherits(x, 'SpatialPixels')) {
			cls <- class(x)
			if (.hasSlot(x, 'data')) {
				x <- as(x, 'SpatialPointsDataFrame')
			} else {
				x <- as(x, 'SpatialPoints')			
			}
			i <- which(!is.na(over(x, e)))
			if (length(i) > 0) {
				x <- x[i,]
				x <- as(x, cls)
				if (draw) {
					points(x, col=col, cex=size)
				}
			} else {
				x <- NULL
			}
		
		} else { # SpatialPoints
		
			i <- which(!is.na(over(x, e)))
			if (length(i) > 0) {
				x <- x[i,]
				if (draw) {
					points(x, col=col, cex=size)
				}
			} else {
				x <- NULL
			}
		}	
		x
	}
)


