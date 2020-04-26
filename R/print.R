# Author: Robert J. Hijmans
# Date :  April 2012
# Version 1.0
# Licence GPL v3



setMethod ('print', 'Raster', 
	function(x, ...) {
		if (inherits(x, 'RasterStack')) {
			show(x)
		} else {
			if (x@file@driver == 'netcdf') {
				nc <- ncdf4::nc_open(x@file@name, suppress_dimvals = TRUE)
				print(nc)
				ncdf4::nc_close(nc)
			} else if (any(is.factor(x))) {
				cat('factor levels (value attributes)\n')
				f <- x@data@attributes
				for (i in 1:length(f)) {
					ff <- f[[i]]
					if (!is.null(ff)) {
						if (nrow(ff) > 15) { 
							ff <- ff[1:15,]
						}
						print(ff)
					}
				}
			# cat('levels      :' , paste(object@data@levels, collapse=', '), '\n')
			# cat('labels      :' , paste(object@data@labels, collapse=', '), '\n')
			} else {
				methods::callNextMethod(x, ...)
			}
		}
	}
)



setMethod ('show' , 'Spatial', 
	function(object) {
		.printSpatial(object)
	}
)


setMethod ('show' , 'SpatialPoints', 
	function(object) {
		.printSpatial(object)
	}
)

setMethod ('show' , 'SpatialPointsDataFrame', 
	function(object) {
		.printSpatial(object)
	}
)

setMethod ('print' , 'Spatial', 
	function(x, ...) {
		.printSpatial(x)
	}
)	


.printSpatial <- function(x, ...) {
	
	cat('class       :' , class(x), '\n')
	isRaster <- hasData <- FALSE
	nc <- 0
	if (.hasSlot(x, 'data')) {
		nc <- ncol(x@data)
		hasData <- TRUE
	}
	ln <- 1
	if (inherits(x, 'SpatialPixels')) {
		isRaster <- TRUE
		cr <- x@grid@cells.dim
		cat ('dimensions  : ', cr[2], ', ', cr[1], ', ', nrow(x@coords), ', ', nc, '  (nrow, ncol, npixels, nlayers)\n', sep="" ) 
		cs <- x@grid@cellsize
		cat ('resolution  : ', cs[1], ', ', cs[2], '  (x, y)\n', sep="")		

	} else if (inherits(x, 'SpatialGrid')) {
		isRaster <- TRUE
		cr <- x@grid@cells.dim
		cat ('dimensions  : ', cr[2], ', ', cr[1], ', ', prod(cr), ', ', nc, '  (nrow, ncol, ncell, nlayers)\n', sep="" ) 
		cs <- x@grid@cellsize
		cat ('resolution  : ', cs[1], ', ', cs[2], '  (x, y)\n', sep="")		
		
	} else {
		nf <- length(x)
		cat('features    :' , nf, '\n')
	}
	
	e <- bbox(x)
	if (nf > 0) {
		cat('extent      : ' , e[1,1], ', ', e[1,2], ', ', e[2,1], ', ', e[2,2], '  (xmin, xmax, ymin, ymax)\n', sep="")
	}
	
	cat('crs         :' ,.get_projection(x), '\n')
	
	if (hasData) {
		x <- x@data
		maxnl <- 15
		
		if (! isRaster) {
			cat('variables   : ', nc, '\n', sep="" ) 
		}
		if (nc > 0) {
			if (nc > maxnl) {
				x <- x[, 1:maxnl]
			}
			ln <- colnames(x)
			if (nc > maxnl) {
				ln <- c(ln[1:maxnl], '...')
				x <- x[, 1:maxnl]
			}
			wrn <- getOption('warn')
			on.exit(options('warn' = wrn))
			options('warn'=-1) 
						
			# r <- apply(x, 2, range, na.rm=TRUE)
			# can give bad sorting (locale dependent)
			# because as.matrix can add whitespace to numbers
			
			rangefun <- function(x) {
				if(is.factor(x)) { 
					range(as.character(x), na.rm=TRUE)
				} else {
					range(x, na.rm=TRUE)
				}
			}
			r <- sapply(x, rangefun)
			i <- r[1,] == "Inf"
			r[,i] <- NA

			minv <- as.vector(r[1, ])
			maxv <- as.vector(r[2, ])
			if (nc > maxnl) {
				minv <- c(minv, '...')
				maxv <- c(maxv, '...')
			}

			w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
			w[is.na(w)] <- 2
			m <- rbind(ln, minv, maxv)
			
			# a loop because 'width' is not recycled by format
			for (i in 1:ncol(m)) {
				m[,i] <- format(m[,i], width=w[i], justify="right")
			}

			cat('names       :', paste(m[1,], collapse=', '), '\n')
			if (nf > 1) {
				cat('min values  :', paste(m[2,], collapse=', '), '\n')
				cat('max values  :', paste(m[3,], collapse=', '), '\n')
			} else if (nf == 1) {
				cat('value       :', paste(m[2,], collapse=', '), '\n')			
			}
		}	
	}
}


