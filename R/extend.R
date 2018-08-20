# Author: Robert J. Hijmans
# Date : October 2008
# Licence GPL v3
# revised November 2011
# version 1.0



if (!isGeneric("extend")) {
	setGeneric("extend", function(x, y, ...)
		standardGeneric("extend"))
}	

setMethod('extend', signature(x='Extent'), 
# function by Etienne B. Racine
function(x, y, ...) {
	if (length(y) == 1) {
		y <- rep(y, 4)
	} else if (length(y) == 2) {
		y <- rep(y, each=2)
	} else if (! length(y) == 4 ) {
		stop('argument "y" should be a vector of 1, 2, or 4 elements')	
	}
	x@xmin <- x@xmin - y[1]
	x@xmax <- x@xmax + y[2]
	x@ymin <- x@ymin - y[3]
	x@ymax <- x@ymax + y[4]
	methods::validObject(x)
	x
}
)



setMethod('extend', signature(x='Raster'), 
function(x, y, value=NA, filename='', ...) {

	if (is.vector(y)) {
		if (length(y) <= 2) {
			adj <- abs(y) * rev(res(x))
			y <- extent(x)
			y@ymin <- y@ymin - adj[1]
			y@ymax <- y@ymax + adj[1]
			y@xmin <- y@xmin - adj[2]
			y@xmax <- y@xmax + adj[2]
		}
	}
	
	test <- try ( y <- extent(y), silent=TRUE )
	if (class(test) == "try-error") {
		stop('Cannot get an Extent object from argument y')
	}

	filename <- trim(filename)
	
	y  <- alignExtent(y, x)
# only expanding here, not cropping
	y <- union(y, extent(x))
	
	if (nlayers(x) <= 1) {
		out <- raster(x)
		leg <- x@legend
	} else {
		out <- brick(x, values=FALSE)	
		leg <- methods::new('.RasterLegend')
	}
	out@data@names <- names(x)
	out <- setExtent(out, y, keepres=TRUE)
	if (any(is.factor(x))) {
#		if (is.na(value)) { perhaps need to check if value is a level
		levels(out) <- levels(x)
	}

	
	if (nrow(x) == nrow(out) & ncol(x) == ncol(out)) {
		# nothing to do.
		return(x)
	}

	if (! hasValues(x) ) {
		return(out)
	}
	
	dtp <- FALSE
	datatype <- list(...)$datatype
	if (is.null(datatype)) { 
		datatype <- unique(dataType(x))
		if (length(datatype) > 1) {
			datatype <- .commonDataType(datatype)
		}
		dtp <- TRUE
	}

	
	if (canProcessInMemory(out)) {
	
		d <- matrix(value, nrow=ncell(out), ncol=nlayers(x))
		d[cellsFromExtent(out, extent(x)), ] <- getValues(x)
		x <- setValues(out, d)	
		if (filename != '') {
			if (dtp) {
				x <- writeRaster(x, filename=filename, datatype=datatype, ...)
			} else {
				x <- writeRaster(x, filename=filename, ...)
			}
		}
		return(x)
		
	} else { 
	
		tr <- blockSize(out)
		tr$old <- rep(TRUE, tr$n)
		startrow <- rowFromY(out, yFromRow(x, 1))
		endrow <- rowFromY(out, yFromRow(x, nrow(x)))
		if (endrow < nrow(out) | startrow > 1) {
			if (nrow(out) > endrow) {
				continuerow <- endrow + 1
			} else {
				continuerow <- NULL
			}
			tr$row <- sort(unique(c(tr$row, startrow, continuerow)))
			tr$nrows <- c(tr$row[-1], nrow(out)+1) - tr$row
			tr$n <- length(tr$row)
			tr$old <- (tr$row <= endrow) & ((tr$row+tr$nrows-1) >= startrow)
		}
		startcol <- colFromX(out, xFromCol(x, 1))
		endcol <- colFromX(out, xFromCol(x, ncol(x)))
		
		pb <- pbCreate(tr$n, label='extend', ...)
		if (dtp) {
			out <- writeStart(out, filename=filename, datatype=datatype, ... )
		} else {
			out <- writeStart(out, filename=filename, ... )		
		}
		
		if ((startcol == 1) & endcol == ncol(out)) { # to make it faster for this case
			for (i in 1:tr$n) {
				if (tr$old[i]) {
					d <- getValues(x, (tr$row[i]-startrow+1), tr$nrows[i])
				} else {
					d <- matrix(value, nrow=tr$nrows[i] * ncol(out), ncol=nlayers(out))
				}
				out <- writeValues(out, d, tr$row[i])
				pbStep(pb, i) 			
			}		
		} else {
			for (i in 1:tr$n) {
				d <- matrix(value, nrow=tr$nrows[i] * ncol(out), ncol=nlayers(out))
				if (tr$old[i]) {
					cells <- startcol:endcol + rep((0:(tr$nrows[i]-1)) * ncol(out), each=endcol-startcol+1)
					d[cells, ] <- getValues(x, (tr$row[i]-startrow+1), tr$nrows[i])
				}
				out <- writeValues(out, d, tr$row[i])
				pbStep(pb, i) 			
			}
		}
		pbClose(pb)
		out <- writeStop(out)
		return(out)

	} 
}
)


