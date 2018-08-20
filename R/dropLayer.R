# Author: Robert J. Hijmans
# Date : June 2008
# Version 0.9
# Licence GPL v3

 
 if (!isGeneric("dropLayer")) {
	setGeneric("dropLayer", function(x, i, ...)
		standardGeneric("dropLayer"))
}
 

...nameToIndex <- function(name, allnames) {
	# this is the same as match, I think
	k = NULL
	for (i in 1:length(name)) {
		k = c(k, which(allnames == name[i])[1])
	}
	return(k)
}
 
 
setMethod('dropLayer', signature(x='RasterStack'), 
function(x, i, ...) {
	if (is.character(i)) {
		i = match(i, names(x))
	}
	i <- sort(unique(round(i)))
	i <- i[i > 0 & i <= nlayers(x)]
	if (length(i) > 0) {
		x@layers <- x@layers[-i]
	}
	return(x)
}
)


setMethod('dropLayer', signature(x='RasterBrick'), 
function(x, i, ...) {
	if (is.character(i)) {
		i <- match(i, names(x))
	}
	i <- sort(unique(round(i)))

	nl <- nlayers(x)
	i <- i[i > 0 & i <= nl]
	if (length(i) < 1) {
		return(x)
	} else {
		sel <- which(! 1:nl %in% i )
		if (length(sel) == 0) {
			return(brick(x, values=FALSE))
		} else {
			return(subset(x, sel, ...))
		}
	}
}
)

