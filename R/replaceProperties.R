# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3



'ncol<-' <- function(x, value) {
	dim(x) <- c(nrow(x), value)
	return(x)
}	

'nrow<-' <- function(x, value) {
	dim(x) <- c(value, ncol(x))
	return(x)
}	


'xmin<-' <- function(x, value) {
	if (inherits(x, 'Extent')) {
		x@xmin <- value
	} else {
		x@extent@xmin <- value
	}
	return(x)
}

'xmax<-' <- function(x, value) {
	if (inherits(x, 'Extent')) {
		x@xmax <- value
	} else {
		x@extent@xmax <- value
	}
	return(x)
}

'ymin<-' <- function(x, value) {
	if (inherits(x, 'Extent')) {
		x@ymin <- value
	} else {
		x@extent@ymin <- value
	}
	return(x)
}

'ymax<-' <- function(x, value) {
	if (inherits(x, 'Extent')) {
		x@ymax <- value
	} else {
		x@extent@ymax <- value
	}
	return(x)
}

