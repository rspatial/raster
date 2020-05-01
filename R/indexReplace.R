# Author: Robert J. Hijmans
# Date :  January 2009
# Version 1.0
# Licence GPL v3


setReplaceMethod("[", c("RasterLayer", "RasterLayer", "missing"),
	function(x, i, j, value) {

		i <- crop(i, x)
		
		if (inherits(value, 'RasterLayer')) {
			value <- getValues(value)
		}
		
		if (! hasValues(i) ) {
			i <- cellsFromExtent(x, i)
			
		} else if (compareRaster(x, i, stopiffalse=FALSE, showwarning=FALSE)) {
			i <- as.logical( getValues(i) )
		
		} else {
			j <- as.logical( getValues(i) )
			i <- cellsFromExtent(x, i)[j]
			x[i] <- value
			return(x)
		}		
	
		.replace(x, i, value=value, recycle=1) 
	}
)



setReplaceMethod("[", c("RasterLayer","missing","missing"),
	function(x, i, j, value) {
	
		if (length(value) == ncell(x)) {
			x <- try( setValues(x, value))
		} else if (length(value) == 1) {
			x <- try( setValues(x, rep(value, times=ncell(x))) )
		} else {
			v <- try( vector(length=ncell(x)) )
			if (! inherits(x, "try-error")) {
				v[] <- value
				x <- try( setValues(x, v) )
			}
		}
		if (inherits(x, "try-error")) {
			stop('cannot replace values on this raster (it is too large')
		}
		return(x)
	}
)


.replace <- function(x, i, value, recycle=1) {
	
	if ( is.logical(i) ) {
		i <- which(i)
	} else {
		i <- stats::na.omit(i)
	}
	if (any(i < 1)) {
		if (!all(i < 1)) {stop("you cannot mix negative and positive subscript")}
		j <- i
		i <- 1:ncell(x)
		i <- i[j]
	}

	nl <- nlayers(x)
  # recycling
	if (nl > 1 & recycle > 0) {
		rec2 <- ceiling(nl / recycle)
		if (rec2 > 1) {
			add <- ncell(x)*recycle * (0:(rec2-1))
			i <- as.vector(t((matrix(rep(i, rec2), nrow=rec2, byrow=TRUE)) + add))
		}
	}
	j <- i > 0 & i <= (ncell(x)*nl)
	
	if (!all(j)) {
		i <- i[j]
		if (length(value) > 1) {
			value <- value[j]
		}
	}


	if ( inMemory(x) ) {
		if (inherits(x, 'RasterStack')) {
			x <- brick( x, values=TRUE )
			# this may go to disk, hence we check again below
		}	
	}
	
	if ( inMemory(x) & hasValues(x) ) {
		x@data@values[i] <- value
		x <- setMinMax(x)
		x <- .clearFile(x)
		return(x)
		
	} else if (canProcessInMemory(x)) {
		if (inherits(x, 'RasterStack')) {
			x <- brick( x, values=TRUE )
			if (!inMemory(x)) {
				x <- readAll(x) 
			}
			x <- .clearFile(x)
			x@data@values[i] <- value
			x <- setMinMax(x)			
		} else if ( fromDisk(x) ) {
			x <- readAll(x)
			x <- .clearFile(x)
			x@data@values[i] <- value
			x <- setMinMax(x)
		} else {
			vals <- rep(NA, times=ncell(x))
			vals[i] <- value
			x <- setValues(x, vals)
		}
		return(x)
			
	} else {
	
		tr <- blockSize(x)
		pb <- pbCreate(tr$n, label='replace')
		hv <- hasValues(x)
		if (nl==1) {
			if (! length(value) %in% c(1, length(i))) {
				stop('cannot replace values in large Raster objects if their length is not 1 or the number of cells to be replaced')
			}
			r <- raster(x)
			r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
			for (k in 1:tr$n) {
				# cells <- cellFromRowCol(x, tr$row[k], 1):cellFromRowCol(x, tr$row[k]+tr$nrows[k]-1, ncol(x))
				cell1 <- cellFromRowCol(x, tr$row[k], 1)
				cell2 <- cell1 + tr$nrows[k] * ncol(x) - 1
				if (hv) {
					v <- getValues(x, row=tr$row[k], nrows=tr$nrows[k])
				} else {
					v <- rep(NA, 1+cell2-cell1)
				}
				j <- which(i >= cell1 & i <= cell2)
				if (length(j) > 0) {
					localcells <- i[j] - (cell1-1)
					if (length(value) == length(i)) {
						v[localcells] <- value[j]
					} else {
						v[localcells] <- value
					}
				}
				r <- writeValues(r, v, tr$row[k])
				pbStep(pb, k) 	
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)
				
		} else {
			if (! length(value) %in% c(1, length(i))) {
				stop('length of replacement values does not match the length of the index')
			}
			r <- brick(x, values=FALSE)
			r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
#			add <- (0:(nl-1)) * ncell(x)
# remove the added cells again....

			nc <- ncol(x)
			ii <- (i-1) %% ncell(x) + 1
			for (k in 1:tr$n) {
				startcell <- cellFromRowCol(x, tr$row[k], 1)
				endcell <- cellFromRowCol(x, tr$row[k]+tr$nrows[k]-1, ncol(x)) 
				if (hv) {
					v <- getValues(x, row=tr$row[k], nrows=tr$nrows[k])
				} else {
					v <- matrix(NA, nrow=tr$nrows[k] * nc, ncol=nl)
				}

				j <- i[ii >= startcell & ii <= endcell] - startcell + 1
				if (length(j) > 0) {
					jj <- (j %/% ncell(x)) * tr$nrow[k] * ncol(x) + (j %% ncell(x))
					if (length(value) == length(i)) {
						v[jj] <- value[jj]
					} else {
						v[jj] <- value
					}
				}
				r <- writeValues(r, v, tr$row[k])
				pbStep(pb, k)
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)
		}	
	}
}


