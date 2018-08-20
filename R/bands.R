# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3



if (!isGeneric("bandnr")) {
	setGeneric("bandnr", function(x, ...)
		standardGeneric("bandnr"))
}	


setMethod('bandnr', signature(x='RasterLayer'), 
function(x) {
	return(x@data@band)
}
)


nbands <- function(x) {
	cx = class(x)
	if (inherits(x, "RasterLayer") | inherits(x, "RasterBrick")) {
		return(x@file@nbands)
	} else {
		stop(paste("not implemented for", class(x), "objects"))
	}	
}


.bandOrder <- function(x) {
	if (inherits(x, "RasterStack")) {
		stop(paste("not implemented for RasterStack objects"))
	} else {
		return(paste(x@file@bandorder))
	}
}

