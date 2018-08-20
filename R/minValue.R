# raster package
# Authors: Robert J. Hijmans
# Date : September 2009
# Version 1.0
# Licence GPL v3



if (!isGeneric("minValue")) {
	setGeneric("minValue", function(x, ...)
		standardGeneric("minValue"))
}	

setMethod('minValue', signature(x='RasterLayer'), 
	function(x, layer=-1, warn=TRUE) {
		if ( x@data@haveminmax ) {
			v <- x@data@min
			if (isTRUE( v == Inf)) {
				v <- NA
			} else {
				if (! inMemory(x) ) {
					v <- v * x@data@gain + x@data@offset
				}
			}
			return(v)
		} else {
			if (warn) warning('min value not known, use setMinMax')
			return(NA)
		}
	}
)


setMethod('minValue', signature(x='RasterBrick'), 
	function(x, layer=-1, warn=FALSE) {
		layer <- round(layer)[1]
		if (layer < 1) { 
			if ( x@data@haveminmax ) {
				v <- x@data@min
				v[v == Inf] <- NA
				if (! inMemory(x) ) {
					v <- v * x@data@gain + x@data@offset
				}	
				return(v)
			} else {
				warning('min value not known, use setMinMax')
				return(rep(NA, nlayers(x)))
			}
		} else {
			if ( x@data@haveminmax ) {
				v <- x@data@min[layer] * x@data@gain + x@data@offset
				v[v == Inf] <- NA
				return(v)
			} else {
				warning('min value not known, use setMinMax')
				return(NA)
			}
		}
	}
)


setMethod('minValue', signature(x='RasterStack'), 
	function(x, layer=-1, warn=FALSE) {
		layer <- round(layer)[1]
		nl <- nlayers(x)
		if (layer < 1) { 
			v <- vector(length=nl)
			for (i in 1:nl) {
				v[i] <- minValue(x@layers[[i]], warn=warn)
			}		
		} else {
			if (layer <= nl) {
				v <- minValue(x@layers[[layer]])
			} else {
				stop('incorrect layer number')
			}
		}
		return(v)
	}
)




if (!isGeneric("maxValue")) {
	setGeneric("maxValue", function(x, ...)
		standardGeneric("maxValue"))
}	

setMethod('maxValue', signature(x='RasterLayer'), 
	function(x, layer=-1, warn=TRUE) {

		if ( x@data@haveminmax ) {
			v <- x@data@max
			if (isTRUE( v == -Inf)) {
				v <- NA
			} else {
				if (! inMemory(x) ) {
					v <- v * x@data@gain + x@data@offset
				}
			}
			return(v)
			
		} else {
			if (warn) warning('max value not known, use setMinMax')
			return(NA)
		}
	}
)

setMethod('maxValue', signature(x='RasterBrick'), 
	function(x, layer=-1, warn=FALSE) {
	
		if ( x@data@haveminmax ) {
			v <- x@data@max
			v[!is.finite(v)] <- NA
			if (! inMemory(x) ) {
				v <- v * x@data@gain + x@data@offset
			}	
			return(v)
		} else {
			if (warn) warning('max value not known, use setMinMax')
			v <- rep(NA, nlayers(x))
		}
		layer <- round(layer)[1]
		if (layer > 0) {
			if (layer <= nlayers(x)) {
				v <- v[layer]
			} else {
				stop('invalid layer selected')
			}
		}
		return(v)
	}
)


		

setMethod('maxValue', signature(x='RasterStack'), 
	function(x, layer=-1, warn=FALSE) {
		layer <- round(layer)[1]
		nl <- nlayers(x)
		if (layer < 1) { 
			v <- vector(length=nl)
			for (i in 1:nl) {
				v[i] <- maxValue(x@layers[[i]], warn=warn)
			}		
		} else {
			if (layer <= nl) {
				v <- maxValue(x@layers[[layer]])
			} else {
				stop('incorrect layer number')
			}
		}
		return(v)
	}
)

