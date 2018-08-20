# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3

setMethod('getValues', signature(x='RasterStack', row='numeric', nrows='missing'), 
	function(x, row, nrows) {
		getValues(x, row=row, nrows=1)
	}
)

setMethod('getValues', signature(x='RasterStack', row='numeric', nrows='numeric'), 
function(x, row, nrows) {
	for (i in 1:nlayers(x)) {
		if (i==1) {
			v <- getValues(x@layers[[i]], row, nrows)
			res <- matrix(ncol=nlayers(x), nrow=length(v))
			res[,1] <- v
		} else {
			res[,i] <- getValues(x@layers[[i]], row, nrows)
		}
	}
	colnames(res) <- names(x)
	res
}
)

setMethod('getValues', signature(x='RasterLayer', row='numeric', nrows='missing'), 
	function(x, row, nrows) {
		getValues(x, row=row, nrows=1)
	}
)

setMethod('getValues', signature(x='RasterLayer', row='numeric', nrows='numeric'), 
function(x, row, nrows, format='') {
	row <- round(row)
	nrows <- round(nrows)
	stopifnot(validRow(x, row))
	stopifnot(nrows > 0)
	row <- min(x@nrows, max(1, row))
	endrow <- max(min(x@nrows, row+nrows-1), row)
	nrows <- endrow - row + 1
	
	if (inMemory(x)){
		startcell <- cellFromRowCol(x, row, 1)
		endcell <- cellFromRowCol(x, row+nrows-1, x@ncols)
		v <-  x@data@values[startcell:endcell] 
	} else if ( fromDisk(x) ) {
		v <- .readRasterLayerValues(x, row, nrows) 
	} else {
		v <- rep(NA, nrows * x@ncols) 
	}
	if (format=='matrix') { 
		v <- matrix(v, nrow=nrows, byrow=TRUE) 
		rownames(v) <- row:(row+nrows-1)
		colnames(v) <- 1:ncol(v)
	} 
	return(v)
}
)

setMethod('getValues', signature(x='RasterBrick', row='numeric', nrows='missing'), 
	function(x, row, nrows) {
		getValues(x, row=row, nrows=1)
	}
)

setMethod('getValues', signature(x='RasterBrick', row='numeric', nrows='numeric'), 
function(x, row, nrows) {

	if (! validRow(x, row)) { 
		stop(row, ' is not a valid rownumber') 
	}
	row <- min(x@nrows, max(1, round(row)))
	endrow <- max(min(x@nrows, row+round(nrows)-1), row)
	nrows <- endrow - row + 1

	if ( inMemory(x) ){
		startcell <- cellFromRowCol(x, row, 1)
		endcell <- cellFromRowCol(x, row+nrows-1, x@ncols)
		res <- x@data@values[startcell:endcell, ,drop=FALSE]
	} else if (fromDisk(x)) {
		res <- .readRasterBrickValues(x, row, nrows)
	} else {
		res <- matrix(NA, nrow=nrows*ncol(x), ncol=nlayers(x))
	}
	colnames(res) <- names(x)
	res
}
)



setMethod('getValues', signature(x='RasterLayerSparse', row='numeric', nrows='missing'), 
	function(x, row, nrows) {
		getValues(x, row=row, nrows=1)
	}
)


setMethod('getValues', signature(x='RasterLayerSparse', row='numeric', nrows='numeric'), 
function(x, row, nrows, format='') {
	row <- round(row)
	nrows <- round(nrows)
	stopifnot(validRow(x, row))
	stopifnot(nrows > 0)
	row <- min(x@nrows, max(1, row))
	endrow <- max(min(x@nrows, row+nrows-1), row)
	nrows <- endrow - row + 1
	nc <- ncol(x)
	
	startcell <- cellFromRowCol(row, 1)
	lastcell <- cellFromRowCol(endrow, nc)
	
	if (inMemory(x)){
		i <- which(x@index >= startcell & x@index <= lastcell)
		if (length(i) > 0) {
			v <- cellFromRowColCombine(x, row:endrow, 1:nc)
			m <- match(i, v)
			v[] <- NA
			v[m] <- x@data@values[i]	
		} else {
			v <- rep(NA, nrows * x@ncols) 
		}
	} else if ( fromDisk(x) ) {
		# not yet implemented
		## v <- .readRasterLayerValues(x, row, nrows) 
	} else {
		v <- rep(NA, nrows * x@ncols) 
	}
	if (format=='matrix') { 
		v <- matrix(v, nrow=nrows, byrow=TRUE) 
		rownames(v) <- row:(row+nrows-1)
		colnames(v) <- 1:ncol(v)
	} 
	return(v)
}
)