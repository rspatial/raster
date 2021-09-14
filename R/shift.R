# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3

	


setMethod('shift', signature(x='Raster'), 
	function(x, dx=0, dy=0, filename='', ...) {
		dx <- as.numeric(dx[1])
		dy <- as.numeric(dy[1])
		stopifnot(!is.na(dx) | !is.na(dy))
		e <- x@extent
		e@xmin <- e@xmin + dx
		e@ymin <- e@ymin + dy
		e@xmax <- e@xmax + dx
		e@ymax <- e@ymax + dy
		x@extent <- e
		if (filename != '') {
			x <- writeRaster(x, filename=filename, ...)
		}
		if (inherits(x, 'RasterStack')) {
			x@layers <- sapply(x@layers, function(i){ extent(i) <- e; i})
		}
		return(x)
	}
)



setMethod('shift', signature(x='SpatialPolygons'), 
	function(x, dx=0, dy=0, ...) {
		a <- data.frame(geom(x))
		a$x <- a$x + dx
		a$y <- a$y + dy
		a <- as(a, 'SpatialPolygons')			
		crs(a) <- crs(x)
		if (inherits(x, 'SpatialPolygonsDataFrame')) {
			a <- sp::SpatialPolygonsDataFrame(a, x@data, match.ID = FALSE)	
		}
		return(a)
	}
)



setMethod('shift', signature(x='SpatialLines'), 
	function(x, dx=0, dy=0, ...) {
		a <- data.frame(geom(x))
		a$x <- a$x + dx
		a$y <- a$y + dy
		a <- as(a, 'SpatialLines')					
		crs(a) <- crs(x)		
		if (inherits(x, 'SpatialLinesDataFrame')) {
			a <- sp::SpatialLinesDataFrame(a, x@data, match.ID = FALSE)	
		}
		return(a)
	}
)


setMethod('shift', signature(x='SpatialPoints'),
    function(x, dx=0, dy=0, ...) {
		x@coords[,1] <- x@coords[,1] + dx
		x@coords[,2] <- x@coords[,2] + dy
        return(x)
    }
)

