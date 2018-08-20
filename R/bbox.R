# R function for the raster package
# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3


setMethod('bbox', signature(obj='Extent'), 
	function(obj) {
		bb <- matrix(ncol=2, nrow=2)
		colnames(bb) <- c("min","max")
		rownames(bb) <- c("s1","s2")
		bb[1,1] <- obj@xmin
		bb[1,2] <- obj@xmax
		bb[2,1] <- obj@ymin
		bb[2,2] <- obj@ymax
		return(bb)
	}	
)

setMethod('bbox', signature(obj='Raster'), 
	function(obj) {
		obj <- extent(obj)
		return( bbox(obj) )
	}	
)

