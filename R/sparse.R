#to be removed

#setAs('RasterLayerSparse', 'RasterLayer', function(from){ raster(from) } )

setClass ("RasterLayerSparse",
	contains = "RasterLayer",
	representation (
		index = "vector"
	),
	prototype (
		index = vector(mode="numeric")
	)
)	

setMethod('raster', signature(x='RasterLayerSparse'), 
	function(x) {
		r <- raster(x@extent, nrows=x@nrows, ncols=x@ncols, crs=.getCRS(x))
		if (length(stats::na.omit(x@data@values)) > 0) {
			v <- rep(NA, ncell(r))
			v[x@index] <- x@data@values
			setValues(r, v)
		} else {
			r
		}
	}
)


setClass (".RasterBrickSparse",
	contains = "RasterBrick",
	representation (
		index = "vector"
	),
	prototype (
		index = vector(mode="numeric")
	)
)	



setAs('RasterLayer', 'RasterLayerSparse', 
	function(from){ 
		x <- methods::new('RasterLayerSparse')
		v <- stats::na.omit(cbind(1:ncell(from), getValues(from)))
		setValues(x, v[,2], v[,1])
	}
)




setMethod("Arith", signature(e1='RasterLayerSparse', e2='numeric'),
    function(e1, e2){ 
	
		if (!hasValues(e1)) { stop('RasterLayerSparse has no values') }
		stopifnot(length(e2) == 1)
		setValues(e1,  methods::callGeneric(as.numeric(e1@data@values), e2))
	}
)

setMethod("Arith", signature(e1='numeric', e2='RasterLayerSparse'),
    function(e1, e2){ 
		if (!hasValues(e2)) { stop('RasterLayerSparse has no values') }
		stopifnot(length(e1) == 1)
		setValues(e2,  methods::callGeneric(as.numeric(e2@data@values), e1) )
	}
)




setMethod("Math", signature(x='RasterLayerSparse'),
    function(x){ 

		if (!hasValues(x)) {
			return(x)
		}
#		funname <- as.character(sys.call(sys.parent())[[1]])
		funname <- .Generic


		if (substr(funname, 1, 3) == 'cum' ) { 
			setValues(x, do.call(funname, list(x@data@values)))
		} else {
			setValues(x, methods::callGeneric(x@data@values))
		}
	}
)



setMethod('setValues', signature(x='RasterLayerSparse'), 

	function(x, values, index=NULL, ...) {
	
		stopifnot(is.vector(values)) 
		if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
			stop('values must be numeric, integer or logical.')	
		}		
		if (is.null(index)) {
			if (! hasValues(x)) {
				stop('you must supply an index argument if the RasterLayerSparse does not have values')
			}
			stopifnot(length(x@index) == length(values)) 
		} else {
			stopifnot(is.vector(index))
			stopifnot(length(index) == length(values)) 
			stopifnot(all(index > 0 | index <= ncell(x)))
			x@index <- index
		}
		x@data@inmemory <- TRUE
		x@data@fromdisk <- FALSE
		x@file@name <- ""
		x@file@driver <- ""
		x@data@values <- values
		x <- setMinMax(x)
		return(x)
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
setMethod('getValuesBlock', signature(x='RasterLayerSparse'), 
 	function(x=1, row, nrows=1, col=1, ncols=(ncol(x)-col+1), format='', ...) {
		
		row <- max(1, min(x@nrows, round(row[1])))
		lastrow <- min(x@nrows, row + round(nrows[1]) - 1)
		nrows <- lastrow - row + 1
		col <- max(1, min(x@ncols, round(col[1])))
		lastcol <- col + round(ncols[1]) - 1
		ncols <- lastcol - col + 1
		
		startcell <- cellFromRowCol(x, row, col)
		lastcell <- cellFromRowCol(x, lastrow, lastcol)

		if (!(validRow(x, row))) {	stop(paste(row, 'is not a valid rownumber')) }
	
		if ( inMemory(x) ) {
			i <- which(x@index >= startcell & x@index <= lastcell)
			if (length(i) > 0) {
				res <- cellFromRowColCombine(x, row:lastrow, col:lastcol)
				m <- match(i, res)
				res[] <- NA
				res[m] <- x@data@values[i]
			} else {
				res <- rep(NA, nrows * ncols)
			}	
		} else if ( fromDisk(x) ) {
			# not yet implemented
			#if (! fromDisk(x)) {
			#	return(rep(NA, times=(lastcell-startcell+1)))
			#}
			#res <- .readRasterLayerValues(x, row, nrows, col, ncols, is.open)
			
		} else  {
			res <- rep(NA, nrows * ncols)			
		} 
			
	
		if (format=='matrix') {
			res = matrix(res, nrow=nrows , ncol=ncols, byrow=TRUE )
			colnames(res) <- col:lastcol
			rownames(res) <- row:lastrow
		}
		res
	}
	
)



setMethod("getValues", signature(x='RasterLayerSparse', row='missing', nrows='missing'), 
function(x, format='') {
	
	cr <- c(x@ncols, x@nrows)
	
	if ( inMemory(x) ) {
		i <- x@index
		v <- x@data@values
		x <- rep(NA, ncell(x))
		x[i] <- v
	} else if ( fromDisk(x) ) {
		# not yet implemented
		### x <- .readRasterLayerValues(x, 1, x@nrows)
	} else {
		x <- rep(NA, ncell(x))
	}

	if (format=='matrix') { 
		x <- matrix(x, ncol=cr[1], nrow=cr[2], byrow=TRUE) 
	}	

	return( x ) 
}
)

