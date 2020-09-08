# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


if (!isGeneric('setValues')) {
	setGeneric('setValues', function(x, values, ...)
		standardGeneric('setValues')) 
}	

	

setMethod('setValues', signature(x='RasterLayer'), 
function(x, values, ...) {

	if (is.factor(values)) {
		levs <- levels(values)
		d <- dim(values)
		values <- as.integer(values)
		if (!is.null(d)) { 
			dim(values) <- d
		}
		x@data@isfactor <- TRUE
		x@data@attributes <- list(data.frame(ID=1:length(levs), VALUE=levs))
	} 
	if (is.matrix(values)) { 
		if (ncol(values) == x@ncols & nrow(values) == x@nrows) {
			values <- as.vector(t(values)) 
		} else if (ncol(values)==1 | nrow(values)==1) {
			values <- as.vector(values)
		} else {
			stop('cannot use a matrix with these dimensions')
		}
	} else if (!is.vector(values)) { 
		stop('values must be a vector or matrix')
	}
	
	if (!(is.numeric(values) | is.factor(values) | is.logical(values))) {
		stop('values must be numeric, logical or factor')	
	}
	

	if (length(values) == 1) {	
		values <- rep(values, ncell(x))
	}

	if (length(values) == ncell(x)) { 
		x@data@inmemory <- TRUE
		x@data@fromdisk <- FALSE
		x@file@name <- ""
		x@file@driver <- ""
		x@data@values <- values
		x <- setMinMax(x)
		return(x)
		
	} else {
		stop("length(values) is not equal to ncell(x), or to 1") 
	}
}
)
	

setMethod('setValues', signature(x='RasterStack'), 
	function(x, values, layer=-1, ...) {
		if (layer > 0) {
			stopifnot(layer <= nlayers(x))
			x[[layer]] <- setValues(x[[layer]], values, ...)
			return(x)
		} else {
			b <- brick(x, values=FALSE)
			setValues(b, values, ...)
		}
	}	
)
	
	

	
setMethod('setValues', signature(x='RasterBrick'), 
	function(x, values, layer=-1, ...) {
	
	layer <- layer[1]
	
	if (is.array(values) & !is.matrix(values)) {	
		dm <- dim(values)
		if (length(dm) != 3) {
			stop('array has wrong number of dimensions (needs to be 3)')
		}
		dmb <- dim(x)
		transpose <- FALSE
		if (dmb[1] == dm[2] & dmb[2] == dm[1]) {
			#if (dm[1] == dm[2]) { warning('assuming values should be transposed') }
			transpose <- TRUE
		} else if (dmb[1] != dm[1] | dmb[2] != dm[2]) {
			stop('dimensions of array do not match the RasterBrick')
		}
# speed imrovements suggested by Justin  McGrath
# http://pastebin.com/uuLvsrYc
		if (!transpose) {
			values <- aperm(values, c(2, 1, 3))
		}
		attributes(values) <- NULL
		dim(values) <- c(dm[1] * dm[2], dm[3])
###		
		
	} else if ( ! (is.vector(values) | is.matrix(values)) ) {
		stop('values must be a vector or a matrix')
	}
	
	if (!(is.numeric(values) | is.integer(values) | is.logical(values))) {
		stop('values must be numeric, integer or logical.')	
	}

#	rownr <- round(rownr)

	if (layer < 1) {
		if (!is.matrix(values)) {
			values <- matrix(values, nrow=ncell(x), ncol=nlayers(x))
		}
		
		if (nrow(values) == ncell(x)) {

			x@file@name <- ""
			x@file@driver <- ""
			x@data@inmemory <- TRUE
			x@data@fromdisk <- FALSE
			x@data@nlayers <- ncol(values)
			cn <- colnames(values)
			if (!is.null(cn)) {
				names(x) <- cn
			}
			x@data@values <- values
			x <- setMinMax(x)
			 
		} else {
			stop("the size of 'values' is not correct")
		}
		
	} else {
		nlx <- nlayers(x)
		if (nlx==0) { 
			x@data@nlayers <- 1
		}
		bind <- FALSE
		layer <- round(layer)
		if (layer > nlx) { 
			if (layer == nlx + 1) {
				bind <- TRUE
			} else {
				stop('layer number too high') 
			}
		}
		
		if (length(values) == ncell(x)) { 
			if ( inMemory(x) ) { 
				if (bind) {
					x@data@values <- cbind(x@data@values, values)
					x@data@nlayers <- as.integer(x@data@nlayers + 1)
				} else {
					x@data@values[,layer] <- values
				}
				rge <- range(values, na.rm=TRUE)
				x@data@min[layer] <- rge[1]
				x@data@max[layer] <- rge[2]
			} else {
			
				if (canProcessInMemory(x)) {
					if (hasValues(x)) {
						x <- readAll(x)
						x@file@name <- ""
						x@file@driver <- ""
						x@data@inmemory <- TRUE
						x@data@fromdisk <- FALSE						
					} else {
						x@data@values <- matrix(NA, nrow=ncell(x), ncol=nlx)
						x@data@min <- rep(Inf, nlx)
						x@data@max <- rep(-Inf, nlx)
						x@data@haveminmax <- TRUE
						x@data@inmemory <- TRUE
					}
					if (bind) {
						x@data@values <- cbind(x@data@values, values)
						x@data@nlayers <- as.integer(x@data@nlayers + 1)
					} else {
						x@data@values[,layer] <- values
					}
					rge <- range(values, na.rm=TRUE)
					x@data@min[layer] <- rge[1]
					x@data@max[layer] <- rge[2]
					
				} else {
				
					tr <- blockSize(x)
					pb <- pbCreate(tr$n, label='setValues',)
					r <- brick(x)
					nc <- ncol(x)
					if (bind) {
						r@data@nlayers <- as.integer(r@data@nlayers + 1)
						r <- writeStart(r, filename=rasterTmpFile(), format=.filetype(), overwrite=TRUE )
						for (i in 1:tr$n) {
							v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
							v <- cbind(v, values[cellFromRowCol(x, tr$row[i], 1):cellFromRowCol(x, tr$row[i]+tr$nrows[i]-1, nc)])
							r <- writeValues(r, v, tr$row[i])
							pbStep(pb, i) 
						}
					} else {
						r <- writeStart(r, filename=rasterTmpFile(), format=.filetype(), overwrite=TRUE )
						for (i in 1:tr$n) {
							v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
							v[, layer] <- values[cellFromRowCol(x, tr$row[i], 1):cellFromRowCol(x, tr$row[i]+tr$nrows[i]-1, nc)]
							r <- writeValues(r, v, tr$row[i])
							pbStep(pb, i) 
						}
					}
					r <- writeStop(r)
					pbClose(pb)
					return(r)
				}
			}
		} else {
			stop("length(values) is not equal to ncell(x)") 
		}
	}
	return(x)
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

