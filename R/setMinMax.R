# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3



	
setMethod('setMinMax', signature(x='RasterLayer'), 
function(x, ...) {
	#w <- getOption('warn')
	#on.exit(options('warn' = w))
	#options('warn'=-1) 
	
	if ( inMemory(x) ) {
		suppressWarnings(x@data@min <- min(x@data@values, na.rm=TRUE))
		suppressWarnings(x@data@max <- max(x@data@values, na.rm=TRUE))
	} else {
		if (! fromDisk(x)) {
			stop('no values associated with this RasterLayer')
		}
		x@data@min <- Inf
		x@data@max <- -Inf
		tr <- blockSize(x)
		pb <- pbCreate(tr$n)	
		x <- readStart(x)	
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i]) 
			x@data@min <- suppressWarnings(min(x@data@min, min(v, na.rm=TRUE)))
			x@data@max <- suppressWarnings(max(x@data@max, max(v, na.rm=TRUE)))
		}
		x <- readStop(x)
	}
	
#	if (datatype == 'logical') {
#		x@data@min <- as.logical(x@data@min)
#		x@data@max <- as.logical(x@data@max)
#	}

	x@data@haveminmax <- TRUE
	return(x)
}
)


setMethod('setMinMax', signature(x='RasterBrick'), 
function(x, ...) {
	
	inMem <- inMemory(x)

	if ( ! inMem ) {
		if (! fromDisk(x) ) {
			stop('no values associated with this RasterBrick')
		}
	} else if (canProcessInMemory(x, (2 + nlayers(x)))) {
		inMem <- TRUE
	}

	w <- getOption('warn')
	on.exit(options('warn' = w))
	options('warn'=-1) 
	
	if ( inMem ) {
	
		rge <- apply( getValues(x), 2, FUN=function(x){ c(min(x, na.rm=TRUE), max(x, na.rm=TRUE)) } )
		x@data@min <- as.vector(rge[1,])
		x@data@max <- as.vector(rge[2,])
		
	} else {
	
		minv <- rep(Inf, nlayers(x))
		maxv <- rep(-Inf, nlayers(x))
		minmax <- rbind(minv, maxv)
		
		tr <- blockSize(x)
		x <- readStart(x)	
		for (i in 1:tr$n) {		
			rsd <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			minmax[1,] <- apply(rbind(rsd, minmax[1,]), 2, min, na.rm=TRUE)
			minmax[2,] <- apply(rbind(rsd, minmax[2,]), 2, max, na.rm=TRUE)
		}
		x@data@min <- minmax[1,]
		x@data@max <- minmax[2,]
		x <- readStop(x)

	}
#	if (datatype == 'logical') {
#		x@data@min <- as.logical(x@data@min)
#		x@data@max <- as.logical(x@data@max)
#	}

	x@data@haveminmax <- TRUE
	return(x)
}
)


setMethod('setMinMax', signature(x='RasterStack'), 
	function(x, ...) {
		for (i in 1:nlayers(x)) {
			x@layers[[i]] <- setMinMax(x@layers[[i]])
		}
		return(x)
	}
)


.haveMinMax <- function(x) {
	if (inherits(x, "RasterLayer") || inherits(x, "RasterBrick")) {
		return(x@data@haveminmax)
	} else if (inherits(x, "RasterStack")) {
		return(all(sapply(x@layers, function(y) y@data@haveminmax)))
	} else {
		return(FALSE)
	}
}
