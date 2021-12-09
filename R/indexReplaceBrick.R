# Author: Robert J. Hijmans
# Date :  January 2009
# Version 1.0
# Licence GPL v3

setMethod("$", "Raster",  function(x, name) { x[[name]] } )

setMethod("$<-", "Raster",  
	function(x, name, value) { 
		i <- which(name == names(x))[1]
		if (is.na(i)) {
			if (inherits(value, 'Raster')) {
				names(value) <- name
				x <- addLayer(x, value)
				return(x)
			} else {
				r <- raster(x)
				names(r) <- name
				r[] <- value
				x <- addLayer(x, r)
				return(x)
			}
		} else {
			if (inherits(value, 'RasterLayer')) {
				x <- value
			} else if (inherits(value, 'Raster')) {
				x[[name]] <- value
			} else {
				r <- x[[name]]
				r[] <- value
				x[[name]] <- value
			}
			return(x)
		} 
	}
)


setMethod("[[", "Raster",
function(x,i,j,...,drop=TRUE) {
	if ( missing(i)) { 
		stop('you must provide an index') 
	}
	if (! missing(j)) { 
		warning('second index is ignored') 
	}
	if (is.numeric(i)) {
		sgn <- sign(i)
		sgn[sgn==0] <- 1
		if (! all(sgn == 1) ) {
			if (! all(sgn == -1) ) {
				stop("only 0's may be mixed with negative subscripts")
			} else {
				i <- (1:nlayers(x))[i]
			}
		}
	}
	subset(x, i, drop=drop)
})


setReplaceMethod("[[", c("RasterStackBrick", "character", "missing"),
	function(x, i, j, value) {
		if (inherits(value, 'Raster')) {
			names(value) <- i
		}
		n <- which(i == names(x))[1]
		if (is.na(n)) {
			n <- nlayers(x) + 1
		} 
		x[[n]] <- value
		x
	}
)

setReplaceMethod("[[", c("RasterLayer", "character", "missing"),
	function(x, i, j, value) {
		stopifnot(i == names(x))
		if (inherits(value, 'RasterLayer')) {
			names(value) <- i
			return(value) 
		} else if (inherits(value, 'Raster')) {
			if (nlayers(value) == 1) {
				value <- value[[1]]
				names(value) <- i
				return(value)
			} else {
				stop("too many layers")
			}
		}
		setValues(x, value)
	}
)

setReplaceMethod("[[", c("RasterStack", "numeric", "missing"),
	function(x, i, j, value) {
	
		i <- round(i)
		if (i < 1) {
			stop('index should be > 0')
		}
		nl <- nlayers(x)
		if (i > nl + 1) {
			stop('index should be <= nlayers(x)+1')
		}
		if (!inherits(value, 'RasterLayer')) {
			val <- value
			if (i > nl) {
				value <- x[[nl]]
			} else {
				value <- x[[i]]
			}
			value[] <- val
		} else {
			compareRaster(x, value)
		}
		
		if (i > nl) {
			x <- addLayer(x, value)
		} else {
			x@layers[[i]] <- value
		}
		x
	}
)



setReplaceMethod("[[", c("Raster", "numeric", "missing"),
	function(x, i, j, value) {
		i <- round(i)

		if (i < 1) {
			stop('index should be > 0')
		}
		nl <- nlayers(x)
		if (i > nl + 1) {
			stop('index should be <= nlayers(x)+1')
		}
		if (inherits(x, "RasterLayer")) {
			return(value)
		}
		
		if (canProcessInMemory(x)) {
			if (!inMemory(x)) {
				x <- readAll(x)
			}
			if (inherits(value, 'RasterLayer')) {
				compareRaster(x, value)
				x <- setValues(x, getValues(value), i)
				names(x)[i] <- names(value)
			} else {
				val <- value
				if (i > nl) {
					value <- getValues(x[[nl]])
				} else {
					value <- getValues(x[[i]])
				}
				# for recycling
				value[] <- val
				x <- setValues(x, value, i)
			}
		} else {
			x <- stack(x)
			x[[i]] <- value
		}	
		return(x)
	}
)


setReplaceMethod("[", c("RasterStackBrick", "Raster", "missing"),
	function(x, i, j, value) {
	
		nl <- nlayers(i)
		if (! hasValues(i) ) {
			i <- cellsFromExtent(x, i)
		} else if (compareRaster(x, i, stopiffalse=FALSE, showwarning=FALSE)) {
			dims <- dim(i)
			i <- as.logical(getValues(i))
			dim(i) <- c(prod(dims[1:2]), dims[3])
		} else {
			i <- cellsFromExtent(x, i)
		}			
		if (nl < nlayers(x)) {
			.replace(x, i, value=value, recycle=nl)
		} else {
			.replace(x, i, value=value, recycle=0) 
		}
	}
)


setReplaceMethod("[", c("Raster", "Extent", "missing"),
	function(x, i, j, value) {
		i <- cellsFromExtent(x, i)
		.replace(x, i, value=value, recycle=1)
	}
)



setReplaceMethod("[", c("Raster", "Spatial", "missing"),
	function(x, i, j, value) {

		if (inherits(i, 'SpatialPolygons')) {
			v <- 1:length(i@polygons)
			v[] <- value
			return( .polygonsToRaster(i, x, field=v, fun='last', mask=FALSE, update=TRUE, updateValue="all", silent=TRUE) )
			
		} else if (inherits(i, 'SpatialLines')) {
			v <- 1:length(i@lines)
			v[] <- value
			return( .linesToRaster(i, x, field=v, fun='last', mask=FALSE, update=TRUE, updateValue="all", silent=TRUE) )
			
		} else { # if (inherits(i, 'SpatialPoints')) {
			i <- cellFromXY(x, sp::coordinates(i)[,1:2,drop=FALSE])

			return( .replace(x, i, value=value, recycle=1) )
		}
	}
)


setReplaceMethod("[", c("RasterStackBrick","missing","missing"),
	function(x, i, j, value) {
	
		nl <- nlayers(x)
		if (inherits(x, 'RasterStack')) {
			x <- brick(x, values=FALSE)
		}
		
		if (is.matrix(value)) {
			if (all(dim(value) == c(ncell(x), nl))) {
				x <- try( setValues(x, value))
			} else {
				stop('dimensions of the matrix do not match the Raster* object')
			}
			
		} else {
			v <- try( matrix(nrow=ncell(x), ncol=nl) )
			if (! inherits(x, "try-error")) {
				v[] <- value
				x <- try( setValues(x, v) )
			}
		}
		if (inherits(x, "try-error")) {
			stop('cannot set values on this raster (it is too large)')
		}
		return(x)
	
	}
)

setReplaceMethod("[", c("Raster", "numeric", "numeric"),
	function(x, i, j, value) {
		i <- cellFromRowColCombine(x, i, j)
		.replace(x, i, value, recycle=1)
	}
)	

setReplaceMethod("[", c("Raster","missing", "numeric"),
	function(x, i, j, value) {
		j <- cellFromCol(x, j)
		.replace(x, j, value=value, recycle=1)
	}
)


setReplaceMethod("[", c("Raster","numeric", "missing"),
	function(x, i, j, value) {
		theCall <- sys.call(-1)
		narg <- length(theCall)-length(match.call(call=sys.call(-1)))
		if (narg > 0) {
			i <- cellFromRow(x, i)
		}
		.replace(x, i=i, value=value, recycle=1)
	}
)


setReplaceMethod("[", c("Raster", "matrix", "missing"),
	function(x, i, j, value) {
		if (ncol(i) == 2) {
			i <- cellFromRowCol(x, i[,1], i[,2])
		} else {
			i <- as.vector(i)
		}
		.replace(x, i=i, value=value, recycle=1)
	}
)



setReplaceMethod("[", c("Raster", "logical", "missing"),
	function(x, i, j, value) {
		.replace(x, i, value, recycle=1)
	}
)	

