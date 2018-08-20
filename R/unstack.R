# Author: Robert J. Hijmans
# Date : March 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("unstack")) {
	setGeneric("unstack", function(x, ...)
		standardGeneric("unstack"))
}	


setMethod("unstack", signature(x='RasterStack'), 
function(x) {
	return(x@layers)
} )


setMethod("unstack", signature(x='RasterBrick'), 
function(x) {
	if (nlayers(x) == 0) { 
		list() 
	} else {
		lapply(1:nlayers(x), function(i) raster(x, i))
	}
} )

