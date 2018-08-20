
metadata <- function(x) {
	x@history
}

'metadata<-' <- function(x, value) {
	stopifnot(is.list(value))
	if (is.data.frame(values)) {
		values <- as.list(values)
	}
	if ( any(unlist(sapply(value, function(x)sapply(x, is.list)))) ) {
		stop('invalid metadata: list is nested too deeply')
	}
	nms <- c(names(value), unlist(sapply(value, names)))
	if (is.null(names) | any(nms == '')) {
		stop('invalid metadata: list elements without names')	
	}
	if (any(unlist(sapply(value, is.data.frame)) )) {
		stop('invalid metadata: data.frames are not allowed')	
	}
	type <- rapply(value, class)
	if (any(type == 'matrix')) {
		stop('invalid metadata: matrices are not allowed')
	}
	x@history <- value
	x
}

