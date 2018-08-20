# Author: Robert J. Hijmans
# Date : January 2009 - December 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("click")) {
	setGeneric("click", function(x, ...)
		standardGeneric("click"))
}	



.getClicks <- function(...) {
	res <- list()
	while(TRUE) {
		loc <- graphics::locator(1, ...)
		if (is.null(loc)) break
		res <- c(res, loc)
	}
	matrix(res, ncol=2, byrow=TRUE)
}



.getCellFromClick <- function(x, n, type, id, ...) {
	loc <- graphics::locator(n, type, ...)
	xyCoords <- cbind(x=loc$x, y=loc$y)
	if (id) {
		text(xyCoords, labels=1:n)
	}
	cells <- cellFromXY(x, xyCoords)
	cells <- unique(stats::na.omit(cells))
	if (length(cells) == 0 ) { 
		stop('no valid cells selected') 
	}
	cells
}



setMethod('click', signature(x='missing'), 
	function(x, n=1, type="n", ...) {
		loc <- graphics::locator(n, type, ...)
		cbind(x=loc$x, y=loc$y)
	}
)

	
setMethod('click', signature(x='SpatialGrid'), 
	function(x, n=1, id=FALSE, xy=FALSE, cell=FALSE, type="n", ...) {
		r <- raster(x)
		cells <- .getCellFromClick(r, n, type, id, ...)
		
		if (.hasSlot(x, 'data')) {
			value <- x@data[cells, ,drop=FALSE]
		} else {
			value <- NULL
		}
		if (cell) {
			value <- data.frame(cells, value)
		}
		if (xy) { 
			xyCoords <- xyFromCell(x, cells)
			colnames(xyCoords) <- c('x', 'y')
			value <- data.frame(xyCoords, value)
		} 
		value
	}
)

setMethod('click', signature(x='SpatialPixels'), 
	function(x, n=1, id=FALSE, xy=FALSE, cell=FALSE, type="n", ...) {
		r <- raster(x)
		cells <- .getCellFromClick(r, n, type, id, ...)
		
		if (.hasSlot(x, 'data')) {
			value <- x@data[cells, ,drop=FALSE]
		} else {
			value <- NULL
		}
		if (cell) {
			value <- data.frame(cells, value)
		}
		if (xy) { 
			xyCoords <- xyFromCell(x, cells)
			colnames(xyCoords) <- c('x', 'y')
			value <- data.frame(xyCoords, value)
		} 
		value
	}
)


.oldclick <- function(x, n=1, id=FALSE, xy=FALSE, cell=FALSE, type="n", ...) {
	
	cells <- .getCellFromClick(x, n, type, id, ...)
	value <- .cellValues(x, cells)
	
	if (is.null(dim(value))) { 
		value <- matrix(value)
		colnames(value) <- names(x)
	}
	if (cell) {
		value <- data.frame(cell=cells, value)
	}
	if (xy) { 
		xyCoords <- xyFromCell(x, cells)
		colnames(xyCoords) <- c('x', 'y')
		value <- data.frame(xyCoords, value)
	} 
	value
}



setMethod('click', signature(x='Raster'), 
	function(x, n=Inf, id=FALSE, xy=FALSE, cell=FALSE, type="n", show=TRUE, ...) {
	values <- NULL
	i <- 0
	n <- max(n, 1)
	while (i < n) {
		i <- i + 1
		loc <- graphics::locator(1, type, ...)
		if (is.null(loc)) break
		xyCoords <- cbind(x=loc$x, y=loc$y)
		if (id) { 
			text(xyCoords, labels=i) 
		}
		cells <- stats::na.omit(cellFromXY(x, xyCoords))
		if (length(cells) == 0) break
		
		value <- extract(x, cells)
		if (cell) {
			value <- data.frame(cell=cells, value)
		}
		if (xy) { 
			xyCoords <- xyFromCell(x, cells)
			colnames(xyCoords) <- c('x', 'y')
			value <- data.frame(xyCoords, value)
		} 
		if (show) {
			print(value)
			utils::flush.console()
		}
		if (is.null(dim(value))) { 
			value <- matrix(value)
			colnames(value) <- names(x)
		}
		values <- rbind(values, value)
	}
	if (show) {
		invisible(values)
	} else {
		values
	}
})




	
setMethod('click', signature(x='SpatialPolygons'),
	function(x, n=1, id=FALSE, xy=FALSE, type="n", ...) {
		loc <- graphics::locator(n, type, ...)
		xyCoords <- cbind(x=loc$x, y=loc$y)
		if (id) {
			text(xyCoords, labels=1:n)
		}

		xyCoords <- SpatialPoints(xyCoords)
		xyCoords@proj4string <- x@proj4string
		i <- which(!is.na(over(x, xyCoords)))
		if (length(i) > 0) {
			if (.hasSlot(x, 'data')) {
				x <- x@data[i,]
			} else {
				x <- row.names(x)[i]
			}
		} else {
			x <- NULL
		}
		
		if (xy) {
			x <- cbind(xyCoords, x)
		}
		return(x)
	}
)


setMethod('click', signature(x='SpatialLines'), 
	function(x, ...) {
		e <- as(drawExtent(), 'SpatialPolygons')
		e@proj4string <- x@proj4string
		i <- which(!is.na(over(x, e)))
		if (length(i) > 0) {
			if (.hasSlot(x, 'data')) {
				x <- x@data[i,]
			} else {
				x <- row.names(x)[i]
			}
		} else {
			x <- NULL
		}
		x
	}
)

setMethod('click', signature(x='SpatialPoints'), 
	function(x, ...) {
		e <- as(drawExtent(), 'SpatialPolygons')
		e@proj4string <- x@proj4string
		i <- which(!is.na(over(x, e)))
		if (length(i) > 0) {
			if (.hasSlot(x, 'data')) {
				x <- x@data[i,]
			} else {
				x <- row.names(x)[i]
			}
		} else {
			x <- NULL
		}
		x
	}
)
