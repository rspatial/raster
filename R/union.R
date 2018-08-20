# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3

if (!isGeneric("union")) {
	setGeneric("union", function(x, y)
		standardGeneric("union"))
}	

setMethod('union', signature(x='Extent', y='Extent'), 
function(x, y) { 
	.unionExtent(x, y)
} )


.unionExtent <- function(x, ...) {
	objects <- c(x, list(...))
	if (length(objects) == 1) {
		return(extent(x))
	}
	e <- extent(objects[[1]])
	for (i in 2:length(objects)) {
		e2 <- extent(objects[[i]])
		e@xmin <- min(e@xmin, e2@xmin)
		e@xmax <- max(e@xmax, e2@xmax)
		e@ymin <- min(e@ymin, e2@ymin)
		e@ymax <- max(e@ymax, e2@ymax)
	}
	return(e)
}

