# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("shift")) {
	setGeneric("shift", function(object, ...)
		standardGeneric("shift"))
}	


setMethod('shift', signature(object='Raster'), 
	function(object, x=0, y=0, filename='', ...) {
		x <- as.numeric(x[1])
		y <- as.numeric(y[1])
		stopifnot(!is.na(x) | !is.na(y))
		e <- object@extent
		e@xmin <- e@xmin + x
		e@ymin <- e@ymin + y
		e@xmax <- e@xmax + x
		e@ymax <- e@ymax + y
		object@extent <- e
		if (filename != '') {
			object <- writeRaster(object, filename=filename, ...)
		}
		if (inherits(object, 'RasterStack')) {
			object@layers <- sapply(object@layers, function(x){ extent(x) <- e; x})
		}
		return(object)
	}
)



setMethod('shift', signature(object='SpatialPolygons'), 
	function(object, x=0, y=0, ...) {
		a <- data.frame(geom(object))
		a$x <- a$x + x
		a$y <- a$y + y
		a <- as(a, 'SpatialPolygons')			
		crs(a) <- crs(object)
		if (inherits(object, 'SpatialPolygonsDataFrame')) {
			a <- SpatialPolygonsDataFrame(a, object@data, match.ID = FALSE)	
		}
		return(a)
	}
)



setMethod('shift', signature(object='SpatialLines'), 
	function(object, x=0, y=0, ...) {
		a <- data.frame(geom(object))
		a$x <- a$x + x
		a$y <- a$y + y
		a <- as(a, 'SpatialLines')					
		crs(a) <- crs(object)		
		if (inherits(object, 'SpatialLinesDataFrame')) {
			a <- SpatialLinesDataFrame(a, object@data, match.ID = FALSE)	
		}
		return(a)
	}
)


setMethod('shift', signature(object='SpatialPoints'),
    function(object, x=0, y=0, ...) {
		object@coords[,1] <- object@coords[,1] + x
		object@coords[,2] <- object@coords[,2] + y
        return(object)
    }
)

