# Author: Robert J. Hijmans
# Date :  June 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("spplot")) {
	setGeneric("spplot", function(obj, ...)
		standardGeneric("spplot"))
}	


setMethod("spplot", signature(obj='Raster'), 
	function(obj, ..., maxpixels=50000, as.table=TRUE, zlim)  {
		obj <- sampleRegular(obj, maxpixels, asRaster=TRUE, useGDAL=TRUE)
		if (!missing(zlim)) {
			if (length(zlim) != 2) {
				warning('zlim should be a vector of two elements')
			} 
			if (length(zlim) >= 2) {
				zlim <- sort(zlim[1:2])
				obj[obj < zlim[1]] <- zlim[1]
				obj[obj > zlim[2]] <- zlim[2]
			}
		}
		obj <- as(obj, 'SpatialGridDataFrame')
		#obj@data <- obj@data[, ncol(obj@data):1]
		spplot(obj, ..., as.table=as.table)
	}
)

# spplot for SpatialPoints object that has no data.frame
setMethod('spplot', signature(obj='SpatialPoints'), 
function(obj, ...) {
	obj <- sp::SpatialPointsDataFrame(obj, data.frame(ID=1:length(obj)))
	spplot(obj, ...)
})

setMethod('spplot', signature(obj='SpatialPolygons'), 
function(obj, ...) {
	obj <- sp::SpatialPolygonsDataFrame(obj, data.frame(ID=1:length(obj)))
	spplot(obj, ...)
})

setMethod('spplot', signature(obj='SpatialLines'), 
function(obj, ...) {
	obj <- sp::SpatialLinesDataFrame(obj, data.frame(ID=1:length(obj)))
	spplot(obj, ...)
})


setMethod("lines", signature(x='SpatialPolygons'),
function(x, ...) {
	x <- as(x, 'SpatialLines')
	lines(x, ...)
}
)


setMethod("spplot", signature(obj='SpatRaster'), 
	function(obj, ..., maxpixels=50000, as.table=TRUE, zlim)  {
		obj <- as(obj, "Raster")
		obj <- sampleRegular(obj, maxpixels, asRaster=TRUE)
		if (!missing(zlim)) {
			if (length(zlim) != 2) {
				warning('zlim should be a vector of two elements')
			} 
			if (length(zlim) >= 2) {
				obj[obj < zlim[1] | obj > zlim[2]] <- NA
			}
		}
		obj <- as(obj, 'SpatialGridDataFrame')
		spplot(obj, ..., as.table=as.table)
	}
)


setMethod("spplot", signature(obj="SpatVector"), 
	function(obj, ...)  {
		x <- as(obj, "Spatial")
		if (.hasSlot(x, "data")) {
			for (i in 1:ncol(x@data)) {
				if (is.character(x@data[,i])) {
					x@data[,i] <- as.factor(x@data[,i])
				}
			}
		}
		spplot(x, ...)
	}
)

