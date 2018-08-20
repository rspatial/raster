# Author: Robert J. Hijmans
# Date : October 2008
# Version 0.9
# Licence GPL v3

# redesinged for multiple row processing
# and arguments ext and overlap
# October 2011
# version 1

if (!isGeneric("merge")) {
	setGeneric("merge", function(x, y, ...)
		standardGeneric("merge"))
}	


setMethod('merge', signature(x='Extent', y='ANY'), 
	function(x, y, ...) {
		x <- c(x, y, list(...))	
		x <- sapply(x, extent)
		x <- x[sapply(x, function(x) inherits(x, 'Extent'))]
		x <- lapply(x, function(e) t(bbox(e)))
		x <- do.call(rbind, x)
		x <- apply(x, 2, range)
		extent(as.vector(x))
	}
)


setMethod('merge', signature(x='RasterStackBrick', y='missing'), 
	function(x, ..., tolerance=0.05, filename="", ext=NULL) {
		nl <- nlayers(x)
		if (nl < 2) {
			return(x)
		} else if (nl == 2) {
			merge(x[[1]], x[[2]], tolerance=tolerance, filename=filename, overlap=TRUE, ext=ext)
		} else {
			do.call(merge, c(x=x[[1]], y=x[[2]], .makeRasterList(x[[3:nl]]), tolerance=tolerance, filename=filename, overlap=TRUE, ext=ext))
		}
	}
)


setMethod('merge', signature(x='Raster', y='Raster'), 

function(x, y, ..., tolerance=0.05, filename="", overlap=TRUE, ext=NULL) { 
	x <- c(x, y, list(...))
	isRast <- sapply(x, function(x) inherits(x, 'Raster'))

	dotargs <- x[ !isRast ]
	x <- x[ isRast ]

	compareRaster(x, extent=FALSE, rowcol=FALSE, orig=TRUE, res=TRUE, tolerance=tolerance)
	
	if (is.null(dotargs$datatype)) {
		dotargs$datatype <- .commonDataType(sapply(x, dataType))  
	}
	filename <- trim(filename)
	dotargs$filename <- filename
	
	nl <- max(unique(sapply(x, nlayers)))
	bb <- .unionExtent(x)
	if (nl > 1) {
		out <- brick(x[[1]], values=FALSE, nl=nl)
	} else {
		out <- raster(x[[1]])
	}
	out <- setExtent(out, bb, keepres=TRUE, snap=FALSE)

	hasV <- sapply(x, hasValues)
	if (!any(hasV)) {
		return(out)
	}

	if (!is.null(ext)) {
		ext <- extent(ext)
		out1 <- extend(out, union(ext, extent(out)))
		out1 <- crop(out1, ext)

		test <- try( intersect(extent(out), extent(out1)) )
		if (class(test) == 'try-error') {
			stop('"ext" does not overlap with any of the input data')
		} 
		out <- out1
		ext <- extent(out)	
	}
			

	
			
	if ( canProcessInMemory(out, 3) ) {
	
		if (!is.null(ext)) {
		
			if (overlap) {
			
				if (nl > 1) {
					v <- matrix(NA, nrow=ncell(out), ncol=nl)
					for (i in 1:length(x)) {
						xy1 <- xyFromCell(x[[i]], 1)
						xy2 <- xyFromCell(x[[i]], ncell(x[[i]]) ) 
						if (xy1[2] > ymin(out) & xy2[2] < ymax(out) & xy1[1] < xmax(out) & xy2[1] > xmin(out)) {		
							cells <- cellsFromExtent( out, extent(x[[i]]) )
							vv <- v[cells, ]
							dat <- extract(x[[i]], ext)
							if (!is.matrix(dat)) {
								dat <- matrix(dat, ncol=1)
							}
							na <- ! rowSums(dat)==nl 
							vv[na, ] <- dat[na, ]
							v[cells, ] <- vv
						}
					}
				} else {
					v <- rep(NA, ncell(out))
					for (i in length(x):1) {
						xy1 <- xyFromCell(x[[i]], 1)
						xy2 <- xyFromCell(x[[i]], ncell(x[[i]]) ) 
						if (xy1[2] > ymin(out) & xy2[2] < ymax(out) & xy1[1] < xmax(out) & xy2[1] > xmin(out)) {		
							cells <- cellsFromExtent( out, extent(x[[i]]) )
							xy <- xyFromCell(out, cells)
							d <- extract(x[[i]], xy)
							j <- !is.na(d)
							v[cells[j]] <- d[j]
						}
					}
				}
				out <- setValues(out, v)
				if (filename != '') {
					dotargs$x <- out
					out <- do.call(writeRaster, dotargs)
				}
				return(out)
				
			} else {  # ignore overlap (if any)
			
				v <- matrix(NA, nrow=ncell(out), ncol=nl)
				for (i in length(x):1 ) {
					xy1 <- xyFromCell(x[[i]], 1)
					xy2 <- xyFromCell(x[[i]], ncell(x[[i]]) ) 
					if (xy1[2] > ymin(out) & xy2[2] < ymax(out) & xy1[1] < xmax(out) & xy2[1] > xmin(out)) {		
						cells <- cellsFromExtent( out, extent(x[[i]]) )
						v[cells, ] <- extract(x[[i]], ext)
					}
				}
				out <- setValues(out, v)
				if (filename != '') {
					dotargs$x <- out
					out <- do.call(writeRaster, dotargs)
				}
				return(out)
			
			}
			
		} else {
		
			if (overlap) {
		
				if (nl > 1) {
					v <- matrix(NA, nrow=ncell(out), ncol=nl)
					for (i in 1:length(x)) {
						cells <- cellsFromExtent( out, extent(x[[i]]) )
						vv <- v[cells, ]
						dat <- getValues(x[[i]])
						if (!is.matrix(dat)) {
							dat <- matrix(dat, ncol=1)
						}
						na <- ! rowSums(is.na(dat)) == nl 
						vv[na, ] <- dat[na, ]
						v[cells, ] <- vv
					}
				} else {
					v <- rep(NA, ncell(out))
					for (i in 1:length(x)) {
						cells <- cellsFromExtent( out, extent(x[[i]]) )
						vv <- v[cells]
						vv[is.na(vv)] <- getValues(x[[i]])[is.na(vv)]
						v[cells] <- vv
					}
				}
				rm(vv)
				out <- setValues(out, v)
				if (filename != '') {
					dotargs$x <- out
					out <- do.call(writeRaster, dotargs)
				}
				return(out)
				
			} else { # no overlap (or ignore overlap)
		
				v <- matrix(NA, nrow=ncell(out), ncol=nl)
				for (i in length(x):1) {
					cells <- cellsFromExtent( out, extent(x[[i]]) )
					v[cells, ] <- getValues(x[[i]])
				}
				out <- setValues(out, v)
				if (filename != '') {
					dotargs$x <- out
					out <- do.call(writeRaster, dotargs)
				}
				return(out)
			}
		}
	}
	
	if (is.null(ext)) {
	
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
	#	tr$row <- sort(unique(c(tr$row, rowcol[,1], rowcol[,2]+1)))
	#	tr$row <- subset(tr$row, tr$row <= nrow(out)) 
	#	tr$nrows <- c(tr$row[-1], nrow(out)+1) - c(tr$row)
	#	tr$n <- length(tr$row)

		pb <- pbCreate(tr$n, dotargs$progress, label='merge')
		dotargs$x <- out
		out <- do.call(writeStart, dotargs)
		
		if (overlap) {
		
			if (nl == 1) {
			
				for (i in 1:tr$n) {
					v <- matrix(NA, nrow=tr$nrow[i], ncol=ncol(out))
					rc <- subset(rowcol, (tr$row[i]+tr$nrow[i]-1) > rowcol[,1] &  tr$row[i] < rowcol[,2])
					if (nrow(rc) > 0) {
						vv <- v
						for (j in nrow(rc):1) {  #reverse order so that the first raster covers the second etc.
							vv[] <- NA
							
							r1 <- tr$row[i]-rc[j,1]+1 
							r2 <- r1 + tr$nrow[i]-1
							z1 <- abs(min(1,r1)-1)+1
							r1 <- max(1, r1)
							r2 <- min(rc[j,6], r2)
							nr <- r2 - r1 + 1
							z2 <- z1 + nr - 1
							
							vv[z1:z2, rc[j,3]:rc[j,4]] <- matrix(getValues(x[[ rc[j,5] ]], r1, nr), nrow=nr, byrow=TRUE)	
							v[!is.na(vv)] <- vv[!is.na(vv)]	
						}
					}
					out <- writeValues(out, as.vector(t(v)), tr$row[i])
					pbStep(pb, i)
				}
				
			} else {
			
				for (i in 1:tr$n) {
					v <- matrix(NA, nrow=tr$nrow[i]*ncol(out), ncol=nl)
					rc <- subset(rowcol, (tr$row[i]+tr$nrow[i]-1) > rowcol[,1] &  tr$row[i] < rowcol[,2])
					if (nrow(rc) > 0) {
						vv <- v
						for (j in nrow(rc):1) { 
							vv[] <- NA

							r1 <- tr$row[i]-rc[j,1]+1 
							r2 <- r1 + tr$nrow[i]-1
							z1 <- abs(min(1,r1)-1)+1
							r1 <- max(1, r1)
							r2 <- min(rc[j,6], r2)
							nr <- r2 - r1 + 1
							z2 <- z1 + nr - 1

							cells <- cellFromRowColCombine(out, z1:z2, rc[j,3]:rc[j,4])
							vv[cells, ] <- getValues(x[[ rc[j,5] ]], r1, nr)					   
							v[!is.na(vv)] <- vv[!is.na(vv)]	
						}
					}
					out <- writeValues(out, v, tr$row[i])
					pbStep(pb, i)
				}
			}
			
		} else { # not overlap
		
			for (i in 1:tr$n) {
				v <- matrix(NA, nrow=tr$nrow[i]*ncol(out), ncol=nl)
				rc <- subset(rowcol, (tr$row[i]+tr$nrow[i]-1) > rowcol[,1] &  tr$row[i] < rowcol[,2])
				if (nrow(rc) > 0) {
					for (j in nrow(rc):1) { 
						r1 <- tr$row[i]-rc[j,1]+1 
						r2 <- r1 + tr$nrow[i]-1
						z1 <- abs(min(1,r1)-1)+1
						r1 <- max(1, r1)
						r2 <- min(rc[j,6], r2)
						nr <- r2 - r1 + 1
						z2 <- z1 + nr - 1
						cells <- cellFromRowColCombine(out, z1:z2, rc[j,3]:rc[j,4])
						v[cells, ] <- getValues(x[[ rc[j,5] ]], r1, nr)					   
					}
					out <- writeValues(out, v, tr$row[i])
					pbStep(pb, i)
				}
			}
		}
		
	} else {  # ext is not null
	
		rowcol <- matrix(NA, ncol=10, nrow=length(x))
		for (i in 1:length(x)) {
			xy1 <- xyFromCell(x[[i]], 1) 				# first row/col on old raster[[i]]
			xy2 <- xyFromCell(x[[i]], ncell(x[[i]]) )   # last row/col on old raster[[i]]
			xyout1 <- xyFromCell(out, 1)
			xyout2 <- xyFromCell(out, ncell(out))
			
			if (xy1[2] > ymin(out) & xy2[2] < ymax(out) & xy1[1] < xmax(out) & xy2[1] > xmin(out)) {
				j <- rowFromY(out, xy1[2])
				rowcol[i,1] <- ifelse(is.na(j), 1, j)    # start row on new raster
				j <- rowFromY(out, xy2[2])
				rowcol[i,2] <- ifelse(is.na(j), nrow(out), j)    # end row
				j <- colFromX(out, xy1[1])
				rowcol[i,3] <- ifelse(is.na(j), 1, j)    # start col
				j <- colFromX(out, xy2[1])
				rowcol[i,4] <- ifelse(is.na(j), ncol(out), j)    # end col
				rowcol[i,5] <- nrow(x[[i]])

				j <- rowFromY(x[[i]], xyout1[2])
				rowcol[i,6] <- ifelse(is.na(j), 1, j)
				j <- rowFromY(x[[i]], xyout2[2])
				rowcol[i,7] <- ifelse(is.na(j), nrow(x[[i]]), j) - rowcol[i,6] + 1
				j <- colFromX(x[[i]], xyout1[1])
				rowcol[i,8] <- ifelse(is.na(j), 1, j)
				j <- colFromX(x[[i]], xyout2[1])
				rowcol[i,9] <- ifelse(is.na(j), ncol(x[[i]]), j) - rowcol[i,8] + 1

				rowcol[i,10] <- i	# layer
				
			}
		}
		rowcol <- subset(rowcol, !is.na(rowcol[,1]))

		tr <- blockSize(out)
	#	tr$row <- sort(unique(c(tr$row, rowcol[,1], rowcol[,2]+1)))
	#	tr$row <- subset(tr$row, tr$row <= nrow(out)) 
	#	tr$nrows <- c(tr$row[-1], nrow(out)+1) - c(tr$row)
	#	tr$n <- length(tr$row)

		pb <- pbCreate(tr$n, dotargs$progress, label='merge')
		dotargs$x <- out
		out <- do.call(writeStart, dotargs)

		if (overlap) {
		
			if (nl == 1) {
				for (i in 1:tr$n) {
					v <- matrix(NA, nrow=tr$nrow[i], ncol=ncol(out))
					rc <- subset(rowcol, (tr$row[i]+tr$nrow[i]-1) > rowcol[,1] &  tr$row[i] < rowcol[,2])
					if (nrow(rc) > 0) {
						vv <- v
						for (j in nrow(rc):1) {  #reverse order so that the first raster covers the second etc.
							vv[] <- NA
							
							r1 <- tr$row[i]-rc[j,1]+rc[j,6]
							r2 <- r1 + tr$nrow[i]-1
							z1 <- abs(min(1,r1)-1)+1
							r1 <- max(1, r1)
							r2 <- min(rc[j,5], r2)
							nr <- r2 - r1 + 1
							z2 <- z1 + nr - 1
							
							vv[z1:z2, rc[j,3]:rc[j,4]] <- matrix(getValuesBlock(x[[ rc[j,10] ]], r1, nr, rc[j,8], rc[j,9]), nrow=nr, byrow=TRUE)	
							v[!is.na(vv)] <- vv[!is.na(vv)]	
						}
					}
					out <- writeValues(out, as.vector(t(v)), tr$row[i])
					pbStep(pb, i)
				}
				
			} else { 
			
				for (i in 1:tr$n) {
					v <- matrix(NA, nrow=tr$nrow[i]*ncol(out), ncol=nl)
					rc <- subset(rowcol, (tr$row[i]+tr$nrow[i]-1) > rowcol[,1] &  tr$row[i] < rowcol[,2])
					if (nrow(rc) > 0) {
						vv <- v
						for (j in nrow(rc):1) { 
							vv[] <- NA

							r1 <- tr$row[i]-rc[j,1]+rc[j,6]
							r2 <- r1 + tr$nrow[i]-1
							z1 <- abs(min(1,r1)-1)+1
							r1 <- max(1, r1)
							r2 <- min(rc[j,5], r2)
							nr <- r2 - r1 + 1
							z2 <- z1 + nr - 1

							cells <- cellFromRowColCombine(out, z1:z2, rc[j,3]:rc[j,4])
							vv[cells, ] <- getValuesBlock(x[[ rc[j,10] ]], r1, nr, rc[j,8], rc[j,9])					   
							v[!is.na(vv)] <- vv[!is.na(vv)]	
						}
					}
					out <- writeValues(out, v, tr$row[i])
					pbStep(pb, i)
				}
			}
			
		}  else {  # no overlap
		
			for (i in 1:tr$n) {
				v <- matrix(NA, nrow=tr$nrow[i]*ncol(out), ncol=nl)
				rc <- subset(rowcol, (tr$row[i]+tr$nrow[i]-1) > rowcol[,1] &  tr$row[i] < rowcol[,2])
				if (nrow(rc) > 0) {
					for (j in nrow(rc):1) { 
						r1 <- tr$row[i]-rc[j,1]+rc[j,6]
						r2 <- r1 + tr$nrow[i]-1
						z1 <- abs(min(1,r1)-1)+1
						r1 <- max(1, r1)
						r2 <- min(rc[j,5], r2)
						nr <- r2 - r1 + 1
						z2 <- z1 + nr - 1

						cells <- cellFromRowColCombine(out, z1:z2, rc[j,3]:rc[j,4])
						v[cells, ] <- getValuesBlock(x[[ rc[j,10] ]], r1, nr, rc[j,8], rc[j,9])					   
					}
				}
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			}
		}
	}

	pbClose(pb)
	writeStop(out)
}
)


