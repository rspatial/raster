# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3

#dataSize <- function(object) {return(object@file@datasize)}
dataSize <- function(object) {
	if (class(object) != 'character'){
		object <- dataType(object)
	}
	return( as.integer (substr(object, 4, 4)) )
}

dataSigned <- function(object) {
	if (class(object) != 'character') { object <- dataType(object) }
	ifelse(substr(object, 5, 5) == 'U', FALSE, TRUE )
}

.shortDataType <- function(object) {
	if (class(object) != 'character') {
		object <- dataType(object)
	}
	return( substr(object, 1, 3)) 
}


dataType <- function(x) {
	if (inherits(x, 'RasterStack')) {
		return(sapply(x@layers, function(x) x@file@datanotation))
	} else {
		return(x@file@datanotation)
	}
}


..dataIndices <- function(object) {
#	return(object@data@indices)
}


fromDisk <- function(x) {
	if (inherits( x, 'RasterStack' )) {
		return( all( sapply( x@layers, function(x) x@data@fromdisk )))
	} else {
		return( x@data@fromdisk )
	}
}
	

setMethod("inMemory", signature(x="BasicRaster"), 
	function(x) {
		if (class(x) == 'BasicRaster') { return(TRUE) }
		if (inherits( x, 'RasterStack' )) {
			return( all( sapply( x@layers, function(x) x@data@inmemory )))
		} else {
			return( x@data@inmemory )
		}
	}
)

setMethod("hasValues", signature(x="BasicRaster"), 
	function(x) {
		if (class(x) == 'BasicRaster') { return(FALSE) }
		if (inherits(x, 'RasterStack')) { 
			if (nlayers(x) > 0) return(TRUE) else return(FALSE)
		}
		if ( fromDisk(x)  | inMemory(x) ) {
			return(TRUE)
		} else {
			return(FALSE)
		}
	}
)
