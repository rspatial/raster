# Author: Robert J. Hijmans
# Date :  June 2008
# Version 1.0
# Licence GPL v3

.naChanged <- function(x) {
	if (.hasSlot(x@file, 'NAchanged')) {
		return(x@file@NAchanged)
	} else {
		return(TRUE)
	}
}

'NAvalue<-' <- function(x, value) {
	if (inherits(x, 'RasterStack')) {
		nl <- nlayers(x)
		if (length(value) == 1) {
			value <- rep(value[[1]], nl)
		} else {
			v <- vector(length=nl)
			v[] <- as.vector(value)
			value <- v
		}
		for (i in 1:nl) {
			x@layers[[i]]@file@nodatavalue <- value[i]
			x@layers[[i]]@file@NAchanged <- TRUE
		}
	} else {
		x@file@nodatavalue <- value[[1]]
		x@file@NAchanged <- TRUE
	}
	return(x)
}

NAvalue <- function(x) {
	if (inherits(x, 'RasterStack')) {
		sapply(x@layers, function(x) { x@file@nodatavalue })
	} else {
		return(x@file@nodatavalue)
	}
}

