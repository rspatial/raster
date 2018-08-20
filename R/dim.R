# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('dim', signature(x='BasicRaster'), 
	function(x){ return(c(nrow(x), ncol(x), 1)) }
)

setMethod('dim', signature(x='RasterStackBrick'), 
	function(x){ return(c(nrow(x), ncol(x), nlayers(x))) }
)


setMethod('nrow', signature(x='BasicRaster'), 
	function(x){ return(x@nrows)}
)

setMethod('ncol', signature(x='BasicRaster'), 
	function(x){ return(x@ncols) }
)




setMethod('dim<-', signature(x='BasicRaster'), 
	function(x, value) {
	
		if (length(value) == 1) {
			value <- c(value, ncol(x))
		} 

		value <- as.integer(pmax(round(value[1:2]), c(1,1)))
		x@nrows <- value[1]
		x@ncols <- value[2]
		
		return(x)	
	}
)


setMethod('dim<-', signature(x='RasterLayer'), 
	function(x, value) {
	
		if (length(value) == 1) {
			value <- c(value, ncol(x))
		} else if (length(value) > 2) {
			value <- value[1:2]
		}
		
		value <- as.integer(pmax(round(value), c(1,1)))
		
		if (value[1] != nrow(x) | value[2] != ncol(x)) {
			x <- clearValues(x)
			x <- .clearFile(x)
			x@nrows <- value[1]
			x@ncols <- value[2]
		}
		return(x)	
	}
)

setMethod('dim<-', signature(x='RasterBrick'), 
	function(x, value) {
	
		if (length(value) == 1) {
			value <- c(value, ncol(x), nlayers(x))
		} else if (length(value) == 2) {
			value <- c(value, nlayers(x))
		} else if (length(value) > 3) {
			warning('value should have length 1, 2, or 3. Additional values ignored')
			value <- value[1:3]
		}
		
		value <- as.integer(pmax(round(value), c(1,1,1)))
		
		if (value[1] != nrow(x) | value[2] != ncol(x) | value[3] != nlayers(x)) {
			x <- clearValues(x)
			x <- .clearFile(x)
			x@nrows <- value[1]
			x@ncols <- value[2]
			x@data@nlayers <- value[3]
		}
		return(x)	
	}
)


