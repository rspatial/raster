# Author: Robert J. Hijmans
# Date :  April 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("ncell")) {
	setGeneric("ncell", function(x)
		standardGeneric("ncell"))
}	

setMethod('ncell', signature(x='BasicRaster'), 
	function(x) {
		return(as.numeric(x@ncols) * x@nrows)
	}
)


setMethod('ncell', signature(x='ANY'), 
	function(x) {
		NROW(x) * NCOL(x)
	}
)



setMethod('length', signature(x='BasicRaster'), 
	function(x) {
		ncell(x) * nlayers(x)
	}
)

