# Robert J. Hijmans
# June 2011
# Version 1.0
# Licence GPL v3



setZ <- function(x, z, name='time') {
	if (is.null(z)) {
		x@z <- list()
		return(x)
	}
	if (is.list(z)) {
		z <- unlist(z)
	}
	stopifnot(length(z) == nlayers(x))
	z <- list(z)
	names(z) <- name[1]
	x@z <- z
	x
}


getZ <- function(x) {
	if (length(x@z) == 0) {
		return(NULL)
	} else {
		return(x@z[[1]])
	}
}


