# Author: Robert J. Hijmans
# Date :  October 2008
# Version 1.0
# Licence GPL v3


if (!isGeneric("nlayers")) {
	setGeneric("nlayers", function(x)
		standardGeneric("nlayers"))
}	

setMethod('nlayers', signature(x='BasicRaster'), 
	function(x){
		return(0) 
    }
)

setMethod('nlayers', signature(x='Raster'), 
	function(x){
		return(1) 
    }
)

setMethod('nlayers', signature(x='RasterStack'), 
	function(x){
		as.integer( sum(unlist( sapply(x@layers, nlayers) ) ) )
    }
)

setMethod('nlayers', signature(x='RasterBrick'), 
	function(x){
		return(x@data@nlayers) 
    }
)

setMethod('nlayers', signature(x='Spatial'), 
	function(x){
		if (! is.null( attr(x, 'data') ) ) {
			return( dim(x@data)[2] ) 
		} else {
			return( 0 )
		}
    }
)

