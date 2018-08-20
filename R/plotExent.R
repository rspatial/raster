# Author: Robert J. Hijmans
# Date :  January 2009
# Version 1.0
# Licence GPL v3


.extentMatrix <- function(x) {
	xy <- matrix(NA, nrow=5, ncol=2)
	xy[c(1,4),1] <- x@xmin
	xy[2:3,1] <- x@xmax
	xy[1:2,2] <- x@ymax
	xy[3:4,2] <- x@ymin
	xy[5,] <- xy[1,]
	return(xy)
}


setMethod("plot", signature(x='Extent', y='missing'), 
	function(x, y, type='l', add=FALSE, ...)  {
		xy <- .extentMatrix(x)
		if (add) {
			lines(xy, ...) 
		} else {
			plot(xy, type=type, ...)
		}
	}
)	

