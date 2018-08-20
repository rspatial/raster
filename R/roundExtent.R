# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod("Math2", signature(x='Extent'), 
	function (x, digits=0) {
		digits <- max(0, round(digits))
		x@xmin <- methods::callGeneric( x@xmin, digits)
		x@xmax <- methods::callGeneric( x@xmax, digits)
		x@ymin <- methods::callGeneric( x@ymin, digits)
		x@ymax <- methods::callGeneric( x@ymax, digits)
		return(x)
	}
)

setMethod("floor", signature(x='Extent'), 
	function (x) {
		x@xmin <- floor( x@xmin)
		x@xmax <- ceiling( x@xmax)
		x@ymin <- floor( x@ymin)
		x@ymax <- ceiling( x@ymax)
		return(x)
	}
)

setMethod("ceiling", signature(x='Extent'), 
	function (x) {
		x@xmin <- ceiling( x@xmin)
		x@xmax <- floor( x@xmax)
		x@ymin <- ceiling( x@ymin)
		x@ymax <- floor( x@ymax)
		return(x)
	}
)

