# Authors: Robert J. Hijmans
# Date :  August 2009
# Version 1.0
# Licence GPL v3



if (!isGeneric('subset')) {
	setGeneric('subset', function(x, ...)
		standardGeneric('subset'))
}

setMethod('subset', signature(x='RasterStack'),
function(x, subset, drop=TRUE, filename='', ...) {
	if (is.character(subset)) {
		i <- stats::na.omit(match(subset, names(x)))
		if (length(i)==0) {
			stop('invalid layer names')
		} else if (length(i) < length(subset)) {
			warning('invalid layer names omitted')
		}
		subset <- i
	}
	subset <- as.integer(subset)
	if (! all(subset %in% 1:nlayers(x))) {
		stop('not a valid subset')
	}

	if (length(subset) == 1 & drop) {
	  if (length(x@z)>0) {
	    z <- lapply(x@z, function(x) x[subset])
	  } else {
	    z <- NULL
	  }
		x <- x@layers[[subset]]
		if (!is.null(z)) {
		  x@z <- z
		}
	} else {
		x@layers <- x@layers[subset]
		if (length(x@z)>0) {
			x@z <- lapply(x@z, function(x) x[subset])
		}
	}
	if (filename != '') {
		x <- writeRaster(x, filename, ...)
	}
	return(x)
} )


setMethod('subset', signature(x='Raster'),
function(x, subset, drop=TRUE, filename='', ...) {


	if (is.character(subset)) {
		i <- stats::na.omit(match(subset, names(x)))
		if (length(i)==0) {
			stop('invalid layer names')
		} else if (length(i) < length(subset)) {
			warning('invalid layer names omitted')
		}
		subset <- i
	}

	subset <- as.integer(subset)
	nl <- nlayers(x)
	if (! all(subset %in% 1:nl)) {
		stop('not a valid subset')
	}

	# now _after_ checking for valid names and adding the possibility to
	# subset a RasterLayer multiple times. Fixed/suggested by Benjamin Leutner
	if (inherits(x, 'RasterLayer')) {
		if (length(subset) > 1) {
		  x <-  stack(lapply(subset, function(...) x))
		}
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)
	}

	#if (nl==1) {return(x)} # this does not drop

	# probably not needed any more, due to removing
	# the filename function below
	#varname <- attr(x@data, "zvar")
	#if (is.null(varname)) {
	#	varname <- ""
	#}

	# these values may have been changed
	# really? how?
	nav <- NAvalue(x)
	e <- extent(x)

	if (fromDisk(x)) {
		nms <- names(x)
		if (drop & length(subset)==1) {
			x <- raster(x, subset)
		} else {
			x <- stack(x, layers=subset)
		}
		extent(x) <- e
		names(x) <- nms[subset]
		NAvalue(x) <- nav
	} else {
		if (drop & length(subset)==1) {
		  if (length(x@z)>0) {
	      z <- lapply(x@z, function(x) x[subset])
	    } else {
	      z <- NULL
	    }
			if (hasValues(x)) {
				x <- raster(x, subset)
			} else {
				x <- raster(x)
			}

		  if (!is.null(z)) {
		    x@z <- z
		  }
			extent(x) <- e
			NAvalue(x) <- nav
			return(x)
		}

		if (hasValues(x)) {
			x@data@values <- x@data@values[, subset, drop=FALSE]
			x@data@min <- x@data@min[subset]
			x@data@max <- x@data@max[subset]
		}
		x@data@names <- x@data@names[subset]
		if (length(x@z) > 0) {
			x@z[[1]] <- x@z[[1]][subset]
		}
		x@data@nlayers <- as.integer(length(subset))
		f <- is.factor(x)
		if (any(f)) {
			x@data@attributes <- x@data@attributes[subset]
			x@data@isfactor <- x@data@isfactor[subset]
		}
	}
	if (filename != '') {
		x <- writeRaster(x, filename, ...)
	}
	x
} )


