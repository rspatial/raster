# Author: Robert J. Hijmans
# Date : June 2008
# Version 0.9
# Licence GPL v3


setMethod('sampleRandom', signature(x='Raster'), 
function(x, size, na.rm=TRUE, ext=NULL, cells=FALSE, rowcol=FALSE, xy=FALSE, sp=FALSE, asRaster=FALSE, ...) {

	if (!hasValues(x)) {
		stop('No values associated with the Raster object')
	}	
	size <- round(size)
	stopifnot(size > 0)
	r <- raster(x)

	if (asRaster) {
		if (! is.null(ext)) {
			x <- crop(x, ext)
		}
		if (size >= ncell(x)) {
			return(x)
		}
		
		if (na.rm) {
			x <- sampleRandom(x, min(ncell(r), size), cells=TRUE, na.rm=TRUE)
			r <- rasterize(xyFromCell(r, x[,1]), r, x[,-1], ...)
		} else {
			cells <- sample(ncell(r), size)
			x <- extract(x, cells)
			r <- rasterize(xyFromCell(r, cells), r, x, ...)
		}
		return(r)
	}
	
	stopifnot(size <= ncell(x))
	nc <- ncell(r)
	layn <- names(x)

	removeCells <- FALSE
	if (sp | rowcol | xy) {
		removeCells <- ! cells
		cells <- TRUE
	}

	if ( canProcessInMemory(x) ) {
	
		if (is.null(ext)) {
			x <- getValues(x)
		} else {
			x <- crop(x, ext)
			rc <- raster(x)
			x <- getValues(x)
		}
		
		if (cells) {
			if (is.null(ext)) {
				x <- cbind(cell=1:nc, value=x)			
			} else {
				XY <- xyFromCell(rc, 1:ncell(rc))
				cell <- cellFromXY(r, XY)
				x <- cbind(cell=cell, x)
			}
		}

		if (na.rm) { 
			x <- stats::na.omit(x)
		}

		if (is.matrix(x)) {
			# get rid of omit attributes
			d <- dim(x)
			x <- matrix(as.vector(x), d[1], d[2])
			if ( nrow(x) > size) {
				s <- sampleInt(nrow(x), size)
				x <- x[s, ,drop=FALSE]
			}
		} else { 
			# get rid of omit attributes
			x <- as.vector(x)
			s <- sampleInt(length(x), size)
			x <- x[s]			
		}
		
	} else {
		
		if (! is.null(ext)) {
			xx <- crop(x, ext)
			nc <- ncell(xx)
			if (size > nc) {
				size <- nc
				warning('size set to the number of cells within "ext": ', size)
			}
		}
			
		if (size >= nc) {
			
			if (is.null(ext)) {
				x <- getValues(x)
			} else {
				r <- raster(x)
				x <- getValues(xx)
			}
			
			if (cells) {
				if (is.null(ext)) {
					x <- cbind(cell=1:nc, value=x)
				} else {
					XY <- xyFromCell(xx, 1:ncell(xx))
					cell <- cellFromXY(r, XY)
					x <- cbind(cell, x)
				}
			}
			if (na.rm) { 
				x <- stats::na.omit(x) 
				# get rid of omit attributes
				if (is.matrix(x)) {
					d <- dim(x)
					x <- matrix(as.vector(x), d[1], d[2])
				} else {
					x <- as.vector(x)
				}
			}
						
		} else {	
		
			if (na.rm) {
				N <- 4 * size 
			} else {
				N <- size 
			}	
			
			N <- min(N, nc)
			rcells <- sampleInt(nc, N)
			
			if (!is.null(ext)) {
				XY <- xyFromCell(xx, rcells)
				rcells <- cellFromXY(r, XY)
			}
			
			x <- .cellValues(x, rcells)
			if (cells) {
				x <- cbind(cell=rcells, value=x)
			}
			
			if (na.rm) {
				x <- stats::na.omit(x)
				if (is.matrix(x)) {
					d <- dim(x)
					x <- matrix(as.vector(x), d[1], d[2])
					if (nrow(x) > size) {
						x <- x[1:size, ]
					}
				} else {
					x <- as.vector(x)
					if ( length(x) > size ) {
						x <- x[1:size]
					}
				}
			}	
		}
		
	} 

	if (is.matrix(x)) {
		if (cells) {
			colnames(x) <- c('cell', layn)
			if (xy) {
				XY <- xyFromCell(r, x[,1])
				x <- cbind(x[,1,drop=FALSE], XY, x[,2:ncol(x),drop=FALSE])
			}
			if (rowcol) {
				rc <- cbind(row=rowFromCell(r, x[,1]), col=colFromCell(r, x[,1]))
				x <- cbind(x[ , 1, drop=FALSE], rc, x[ , 2:ncol(x), drop=FALSE])
			}
			if (sp) {
				if (!xy) {
					XY <- data.frame(xyFromCell(r, x[,1]))
				}
				if (removeCells) {
					x <- x[,-1,drop=FALSE]
				}
				x <- sp::SpatialPointsDataFrame(XY, data=data.frame(x),  proj4string=.getCRS((r)))
				
			} else if (removeCells) {
				x <- x[,-1,drop=FALSE]	
			}
			
		} else {
			colnames(x) <- layn
		}
	}
		
	return(x)
}
)


