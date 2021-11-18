# Author: Robert J. Hijmans
# Date : November 2008
# Version 1.0
# Licence GPL v3



if (!isGeneric("readAll")) {
	setGeneric("readAll", function(object)
		standardGeneric("readAll"))
}

	
setMethod('readAll', signature(object='RasterLayer'), 
	function(object){ 
		if (! object@data@fromdisk)  {
			warning('cannot read values; there is no file associated with this RasterLayer')
			return(object)
		}
		object@data@values <- .readRasterLayerValues(object, 1, object@nrows)
		suppressWarnings(object@data@min <- as.vector( min(object@data@values, na.rm=TRUE ) ))
		suppressWarnings(object@data@max <- as.vector( max(object@data@values, na.rm=TRUE ) ))
		object@data@haveminmax <- TRUE
		object@data@inmemory <- TRUE
		object@data@fromdisk <- FALSE
		object@file@name <- ""
		
		return(object)
	}
)


setMethod('readAll', signature(object='RasterStack'), 
	function(object){ 
		for (i in seq(nlayers(object))) {
			if (! object@layers[[i]]@data@inmemory  ) {
				object@layers[[i]] <- readAll(object@layers[[i]])
#				object@layers[[i]]@data@values <- .readRasterLayerValues(object@layers[[i]], 1, object@nrows) 
			}
		}
		return(object)
	}
)


setMethod('readAll', signature(object='RasterBrick'), 
	function(object){ 
		if (! object@data@fromdisk)  {
			warning('cannot read values; there is no file associated with this RasterBrick')
			return(object)
		}
		object@data@values <- .readRasterBrickValues(object, 1, object@nrows)
		w <- getOption('warn')
		on.exit(options('warn' = w))
		options('warn'=-1) 
		rge <- apply(object@data@values, 2, FUN=function(x){ range(x, na.rm=TRUE) } )
		object@data@min <- as.vector(rge[1,])
		object@data@max <- as.vector(rge[2,])
		object@data@haveminmax <- TRUE
		object@data@inmemory <- TRUE
		object@data@fromdisk <- FALSE
		object@file@name <- ""
		return(object)
	}
)

