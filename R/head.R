# Author: Robert J. Hijmans
# Date : December 2010
# Version 0.9
# Licence GPL v3


setMethod('head', signature(x='RasterLayer'), 
	function(x, cols=20, rows=10, ...) {
		nr <- min(x@nrows, max(1, rows))
		nc <- min(x@ncols, max(1, cols))
		v <- getValuesBlock(x, 1, nrows=nr, ncols=nc, format='matrix')
		return(v)
	}
)

setMethod('tail', signature(x='RasterLayer'), 
	function(x, cols=20, rows=10, ...) {
		nr <- min(x@nrows, max(1, rows))
		nc <- min(x@ncols, max(1, cols))
		sr <- x@nrows - nr + 1
		sc <- x@ncols - nc + 1
		v <- getValuesBlock(x, row=sr, nrows=nr, col=sc, ncols=nc, format='matrix')
		return(v)
	}
)



setMethod('head', signature(x='RasterStackBrick'), 
	function(x, cols=10, rows=2, layers=10, ...) {
		nr <- min(x@nrows, max(1, rows))
		nc <- min(x@ncols, max(1, cols))
		nl <- min(nlayers(x), max(1, layers))
		v <- getValuesBlock(x, 1, nrows=nr, ncols=nc)
		return(v)
	}
)

setMethod('tail', signature(x='RasterStackBrick'), 
	function(x, cols=10, rows=2, layers=10, ...) {
		nr <- min(x@nrows, max(1, rows))
		nc <- min(x@ncols, max(1, cols))
		nl <- min(nlayers(x), max(1, layers))
		sr <- x@nrows - nr + 1
		sc <- x@ncols - nc + 1
		v <- getValuesBlock(x, row=sr, nrows=nr, col=sc, ncols=nc)
		return(v)
	}
)



setMethod('head', signature(x='Spatial'), 
	function(x, n=6L,...) {
		if (.hasSlot(x, 'data')) {
			head(x@data, n=n, ...)
		} else {
			x[1,]
		}
	}
)



setMethod('tail', signature(x='Spatial'), 
	function(x,  n=6L, ...) {
		if (.hasSlot(x, 'data')) {
			tail(x@data, n=n, ...)
		} else {
			x[length(x),]
		}
	}
)


