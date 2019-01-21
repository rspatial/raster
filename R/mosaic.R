# Author: Robert J. Hijmans
# Date : October 2008
# Version 0.9
# Licence GPL v3
# redesigned for multiple row processing
# October 2011
# version 1


setMethod('mosaic', signature(x='Raster', y='Raster'), 
function(x, y, ..., fun, tolerance=0.05, filename="") { 
	x <- c(x, y, list(...))	
	isRast <- sapply(x, function(x) inherits(x, 'Raster'))

	dotargs <- x[ !isRast ]
	x <- x[ isRast ]
	
	if (is.null(dotargs$datatype)) {
		dotargs$datatype <- .commonDataType(sapply(x, dataType))  
	}
	filename <- trim(filename)
	dotargs$filename <- filename

	nl <- max(unique(sapply(x, nlayers)))
	compareRaster(x, extent=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)

	bb <- .unionExtent(x)
	if (nl > 1) {
		out <- brick(x[[1]], values=FALSE, nl=nl)
	} else {
		out <- raster(x[[1]])
	}
	
	out <- setExtent(out, bb, keepres=TRUE, snap=FALSE)

	fun <- .makeTextFun(fun)
	if (class(fun) == 'character') { 
		rowcalc <- TRUE 
		fun <- .getRowFun(fun)
	} else { 
		rowcalc <- FALSE 
	}
	
	if ( canProcessInMemory(out, 2 + length(x)) ) {
		if (nl > 1) {
			v <- matrix(NA, nrow=ncell(out)*nl, ncol=length(x))
			for (i in 1:length(x)) {
				cells <- cellsFromExtent( out, extent(x[[i]]) )
				cells <- cells + rep(0:(nl-1)*ncell(out), each=length(cells))
				v[cells, i] <- as.vector(getValues(x[[i]]))
			}
			if (rowcalc) {
				v <- fun(v, na.rm=TRUE)
			} else {
				v <- apply(v, 1, fun, na.rm=TRUE)
			}
			v <- matrix(v, ncol=nl)	
			
		} else {
		
			v <- matrix(NA, nrow=ncell(out), ncol=length(x))
			for (i in 1:length(x)) {
				cells <- cellsFromExtent( out, extent(x[[i]]) )
				v[cells,i] <- getValues(x[[i]])
			}
			if (rowcalc) {
				v <- fun(v, na.rm=TRUE)
			} else {
				v <- apply(v, 1, fun, na.rm=TRUE)
			}
		}
		out <- setValues(out, v)
		if (filename != '') {
			dotargs$x <- out
			out <- do.call(writeRaster, dotargs)
		}
		return(out)
	}

	rowcol <- matrix(NA, ncol=6, nrow=length(x))
	for (i in 1:length(x)) {
		xy1 <- xyFromCell(x[[i]], 1) 				# first row/col on old raster[[i]]
		xy2 <- xyFromCell(x[[i]], ncell(x[[i]]) )   # last row/col on old raster[[i]]
		rowcol[i,1] <- rowFromY(out, xy1[2])       	# start row on new raster
		rowcol[i,2] <- rowFromY(out, xy2[2])    	# end row
		rowcol[i,3] <- colFromX(out, xy1[1])	    # start col
		rowcol[i,4] <- colFromX(out, xy2[1])		# end col
		rowcol[i,5] <- i							# layer
		rowcol[i,6] <- nrow(x[[i]])
	}

	tr <- blockSize(out)
	pb <- pbCreate(tr$n, dotargs$progress, label='mosaic')

	dotargs$x <- out
	out <- do.call(writeStart, dotargs)

	if (nl == 1) {
		for (i in 1:tr$n) {
			rc <- subset(rowcol, (tr$row[i]+tr$nrow[i]-1) > rowcol[,1] &  tr$row[i] < rowcol[,2])
			if (nrow(rc) > 0) {
				v <- matrix(NA, nrow=tr$nrow[i] * ncol(out), ncol=nrow(rc))
				for (j in 1:nrow(rc)) {
				
					r1 <- tr$row[i]-rc[j,1]+1 
					r2 <- r1 + tr$nrow[i]-1
					z1 <- abs(min(1,r1)-1)+1
					r1 <- max(1, r1)
					r2 <- min(rc[j,6], r2)
					nr <- r2 - r1 + 1
					z2 <- z1 + nr - 1
				
					cells <- cellFromRowColCombine(out, z1:z2, rc[j,3]:rc[j,4])
					v[cells, j] <- getValues(x[[ rc[j,5] ]], r1, nr)
				}
				if (rowcalc) {
					v <- fun(v, na.rm=TRUE)
				} else {
					v <- apply(v, 1, fun, na.rm=TRUE)
				}				
			} else {
				v <- rep(NA, tr$nrow[i] * ncol(out))
			}
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
	} else {
		for (i in 1:tr$n) {
			rc <- subset(rowcol, (tr$row[i]+tr$nrow[i]-1) > rowcol[,1] &  tr$row[i] < rowcol[,2])
			if (nrow(rc) > 0) {
				v <- matrix(NA, nrow=tr$nrow[i]*ncol(out) * nl, ncol=nrow(rc))
				for (j in 1:nrow(rc)) { 

					r1 <- tr$row[i]-rc[j,1]+1 
					r2 <- r1 + tr$nrow[i]-1
					z1 <- abs(min(1,r1)-1)+1
					r1 <- max(1, r1)
					r2 <- min(rc[j,6], r2)
					nr <- r2 - r1 + 1
					z2 <- z1 + nr - 1

					cells <- cellFromRowColCombine(out, z1:z2, rc[j,3]:rc[j,4])
					cells <- cells + rep(0:(nl-1)* tr$nrow[i]*ncol(out), each=length(cells))
					v[cells, j] <- as.vector( getValues(x[[ rc[j,5] ]], r1, nr) )
					
				}
				if (rowcalc) {
					v <- fun(v, na.rm=TRUE)
				} else {
					v <- apply(v, 1, fun, na.rm=TRUE)
				}
				v <- matrix(v, ncol=nl)
			} else {
				v <- matrix(NA, nrow=tr$nrow[i] * ncol(out), ncol=nl)
			}
			
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
	}
	pbClose(pb)
	writeStop(out)
}
)

