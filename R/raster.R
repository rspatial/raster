# Author: Robert J. Hijmans
# Date : September 2008
# Version 1.0
# Licence GPL v3



setMethod('raster', signature(x='missing'), 
	function(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, crs, ext, resolution, vals=NULL) {
		if (missing(ext)) {
			ext <- extent(xmn, xmx, ymn, ymx)
		}
		if (missing(crs)) {
			if (ext@xmin > -360.01 & ext@xmax < 360.01 & ext@ymin > -90.01 & ext@ymax < 90.01) { 
				crs <- CRS("+proj=longlat +datum=WGS84")
			} else {
				# if sp >= 1.2.1  crs <- CRS(as.character(NA), doCheckCRSArgs=FALSE)
				crs <- CRS(as.character(NA), doCheckCRSArgs=FALSE)
			}
		} else {
			crs <- .getCRS(crs)
		}
		if (missing(resolution)) {
			nrows <- as.integer(max(1, round(nrows)))
			ncols <- as.integer(max(1, round(ncols)))
			r <- methods::new('RasterLayer', extent=ext, nrows=nrows, ncols=ncols, crs=crs)
		} else {
			r <- methods::new('RasterLayer', extent=ext, crs=crs)
			res(r) <- resolution
		}
		if (!is.null(vals)) {
			return( setValues(r, vals) )
		} else {
			return( r )
		}
	}
)
  

setMethod('raster', signature(x='list'), 
	function(x, crs) {
	# list should represent an "image"
		if (is.null(x$x)) { stop('list has no "x"') }
		if (is.null(x$y)) { stop('list has no "y"') }
		if (is.null(x$z)) { stop('list has no "z"') }
		if (! all(dim(x$z) == c(length(x$x), length(x$y)))) { stop('"z" does not have the right dimensions') }

		# omitted "-1" bug fix by Barry Rowlingson 
		resx <- ( x$x[length(x$x)] - x$x[1] ) / (length(x$x)-1)
		resy <- ( x$y[length(x$y)] - x$y[1] ) / (length(x$y)-1)
		xmn <- min(x$x) - 0.5 * resx
		xmx <- max(x$x) + 0.5 * resx
		ymn <- min(x$y) - 0.5 * resy
		ymx <- max(x$y) + 0.5 * resy

		
		dx <- abs(max(abs((x$x[-1] - x$x[-length(x$x)])) / resx) - 1)
		dy <- abs(max(abs((x$y[-1] - x$y[-length(x$y)])) / resy) - 1)
		if (is.na(dx) | is.na(dy)) {
			stop('NA values in coordinates')
		} 
		if (dx > 0.01 | dy > 0.01) {
			stop('data are not on a regular grid')
		}
		
		
		if (missing(crs)) {
			if (xmn > -360.1 & xmx < 360.1 & ymn > -90.1 & ymx < 90.1) { 
				crs <- CRS("+proj=longlat +datum=WGS84")
			} else {
				crs <- CRS(as.character(NA))
			}
		} else {
			crs <- .getCRS(crs)
		}
		x <- t(x$z)
		x <- x[nrow(x):1, ]
		r <- raster( x, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, crs=crs )
		
		return(r)
	}
)


setMethod('raster', signature(x='matrix'), 
	function(x, xmn=0, xmx=1, ymn=0, ymx=1, crs="", template=NULL) {
		crs <- .getCRS(crs)
		if (!is.null(template)) {
			if (inherits(template, 'Extent')) {
				r <- raster(template, crs=crs)
			} else {
				r <- raster(template)
			}
		} else {
			r <- raster(ncols=ncol(x), nrows=nrow(x), xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, crs=crs)
		}
		r <- setValues(r, as.vector(t(x)))
		return(r)
	}
)


# setMethod('raster', signature(x='big.matrix'), 
	# function(x, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA, template=NULL) {
		# if (isTRUE(is.na(crs))) {
			# crs <- as.character(NA)
		# }	
		# if (!is.null(template)) {
			# if (inherits(template, 'Extent')) {
				# r <- raster(template, crs=crs)
			# } else {
				# r <- raster(template)
			# }
		# } else {
			# r <- raster(ncols=ncol(x), nrows=nrow(x), xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, crs=crs)
		# }
		
# #		r@file@driver <- 'big.matrix'
# #		if (is.filebacked(x)) {
# #			r@file@name <- bigmemory:::file.name(x)
# #		}
		# r@data@fromdisk <- TRUE
		# r@data@inmemory <- FALSE
		# attr(r@file, 'big.matrix') <- x
		# return(r)
	# }
# )



setMethod('raster', signature(x='character'), 
	function(x, band=1, ...) {
		x <- .fullFilename(x)
		r <- .rasterObjectFromFile(x, band=band, objecttype='RasterLayer', ...)
		return(r)
	}
)


setMethod('raster', signature(x='BasicRaster'), 
	function(x) {
		r <- raster(x@extent, nrows=x@nrows, ncols=x@ncols, crs=.getCRS(x))
		if (rotated(x)) {
			r@rotated <- TRUE
			r@rotation <- x@rotation
		}
		return(r)
	}
)

setMethod('raster', signature(x='RasterLayer'), 
	function(x) {
		r <- raster(x@extent, nrows=x@nrows, ncols=x@ncols, crs=.getCRS(x))
		r@rotated <- x@rotated
		r@rotation <- x@rotation
		r@file@blockrows <- x@file@blockrows
		r@file@blockcols <- x@file@blockcols
		return(r)
	}
)



setMethod('raster', signature(x='RasterStack'), 
	function(x, layer=0){
		newindex = -1
		if (nlayers(x) > 0) {
			if (!is.numeric(layer)) {
				newindex <- which(names(x) == layer)[1]
				if (is.na (newindex) ) { 
					warning('variable', layer, 'does not exist')
					newindex = -1
				} 
				layer <- newindex
			}
		}
		if ( layer > 0 ) {
			dindex <- max(1, min(nlayers(x), layer))
			if (dindex != layer) { warning(paste("layer was changed to", dindex))}
			r <- x@layers[[dindex]]
			names(r) <- names(x)[dindex]
		} else {
			r <- raster(extent(x))
			dim(r) <- c(nrow(x), ncol(x))
			projection(r) <-.getCRS(x)
		}
		extent(r) <- extent(x) # perhaps it was changed by user and different on disk
		if (rotated(x@layers[[1]])) {
			r@rotated <- TRUE
			r@rotation <- x@layers[[1]]@rotation
		}
		
		return(r)
	}
)


setMethod('raster', signature(x='RasterBrick'), 
	function(x, layer=0){
		newindex <- -1
		if (nlayers(x) > 0) {
			if (!is.numeric(layer)) {
				newindex <- which(names(x) == layer)[1]
				if (is.na (newindex) ) { 
					warning('variable', layer, 'does not exist')
					newindex = -1
				} 
				layer <- newindex
			}
			layer <- round(layer)
		}
		if (layer > 0) {
			dindex <- as.integer(max(1, min(nlayers(x), layer)))
			
			if ( fromDisk(x) ) {
			
				if (dindex != layer) { warning(paste("layer was changed to", dindex))}
				
				# better raster(filename(x), band=dindex)  ?
				# with zvar for ncdf files?
				
				r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), crs=.getCRS(x))	
				r@file <- x@file

				r@file@blockrows <- x@file@blockrows
				r@file@blockcols <- x@file@blockcols
				
				r@data@offset <- x@data@offset
				r@data@gain <- x@data@gain
				r@data@inmemory <- FALSE
				r@data@fromdisk <- TRUE
				r@data@haveminmax <- x@data@haveminmax

				r@data@band <- dindex
				r@data@min <- x@data@min[dindex]
				r@data@max <- x@data@max[dindex]
				ln <- x@data@names[dindex]
				if (! is.na(ln) ) { 
					r@data@names <- ln 
				}
				#zv <- unlist(x@z[1])[dindex]
				zv <- NULL
				try( zv <- x@z[[1]][dindex], silent=TRUE )
				if (! is.null(zv) ) { 
					r@z <- list(zv)
				}
				
				# ncdf files
				zvar <- try(methods::slot(x@data, 'zvar'), silent=TRUE)
				if (!(inherits(zvar, "try-error"))) {
					attr(r@data, "zvar") <- zvar
					attr(r@data, "dim3") <- x@data@dim3
					attr(r@data, "level") <- x@data@level
				}

				r@data@offset <- x@data@offset
				r@data@gain <- x@data@gain
				r@file@nodatavalue <- x@file@nodatavalue
				
			} else {
			
				r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), crs=.getCRS(x))	
				if ( inMemory(x) ) {
					if ( dindex != layer ) { warning(paste("layer was changed to", dindex)) }
					r <- setValues(r, x@data@values[,dindex])
					r@data@names <- names(x)[dindex]	
				}
			}
			isf <- is.factor(x)[dindex]
			if (isTRUE(isf)) {
				r@data@isfactor <- TRUE
				r@data@attributes <- levels(x)[dindex]
			}
			
		} else {
			r <- raster(extent(x), nrows=nrow(x), ncols=ncol(x), crs=.getCRS(x))	
		}

		if (rotated(x)) {
			r@rotated <- TRUE
			r@rotation <- x@rotation
		}
	
		return(r)
	}
)


setMethod('raster', signature(x='Extent'), 
	function(x, nrows=10, ncols=10, crs="", ...) {
		crs <- .getCRS(crs)
		raster(xmn=x@xmin, xmx=x@xmax, ymn=x@ymin, ymx=x@ymax, ncols=ncols, nrows=nrows, crs=crs, ...)
	}
)


setMethod('raster', signature(x='sf'), 
	function(x, origin, ...){
		sp <- .sf2sp(x)
		raster(sp, origin, ...)
	}
)


setMethod('raster', signature(x='Spatial'), 
	function(x, origin, ...){
		r <- raster(extent(x), ...)
		crs(r) <- .getCRS(x)
		if (!missing(origin)) {
			origin(r) <- origin
			r <- extend(r, 1)
			r <- crop(r, x, snap='out')
		}
		r
	}
)


setMethod('raster', signature(x='SpatialGrid'), 
	function(x, layer=1, values=TRUE){
		r <- raster(extent(x))
		projection(r) <-.getCRS(x)
		dim(r) <- c(x@grid@cells.dim[2], x@grid@cells.dim[1])	
		if (layer < 1) {
			values <- FALSE
		}
		
		if (inherits(x, 'SpatialGridDataFrame') & values) {
			if (dim(x@data)[2] > 0) {
				layer = layer[1]
				if (is.numeric(layer)) {
					dindex <- max(1, min(dim(x@data)[2], layer))
					if (dindex != layer) {
						warning(paste("layer was changed to: ", dindex))
					}
					layer <- dindex
					names(r) <- colnames(x@data)[layer]
				} else if (!(layer %in% names(x))) {
					stop(layer, ' is not a valid name')
				} else {
					names(r) <- layer
				}

				if (is.character( x@data[[layer]]) ) { 
					x@data[[layer]] <- as.factor(x@data[[layer]])
				}
				if (is.factor( x@data[[layer]]) ) { 
					r@data@isfactor <- TRUE 
					levs <- levels(x@data[[layer]])
					r@data@attributes <- list(data.frame(ID=1:length(levs), levels=levs))
					r <- setValues(r, as.integer(x@data[[layer]]))
				} else {
					r <- setValues(r, x@data[[layer]])
				}
			}
			
		}
		
		return(r)
	}	
)


setMethod('raster', signature(x='SpatialPixels'), 
	function(x, layer=1, values=TRUE){
		if (inherits(x, 'SpatialPixelsDataFrame')) {
			if (layer < 1) {
				x <- as(x, 'SpatialGrid')
			} else {
				x <- as(x[layer], 'SpatialGridDataFrame')
				return(raster(x, values=values))
			}	
		} else {
			x <- as(x, 'SpatialGrid')
			return(raster(x))		
		}
		return(x)
	}
)



setMethod('raster', signature(x='im'), 
	function(x, crs) {
		r <- as(x, 'RasterLayer')
		if (!missing(crs)) {
			projection(r) <- crs
		}
		r
	}
)



setMethod('raster', signature(x='kasc'), 
	function(x, crs) {
		x <- as(x, 'RasterLayer')
		if (missing(crs)) {
			e <- x@extent
			if (e@xmin > -360.1 & e@xmax < 360.1 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs <- CRS("+proj=longlat +datum=WGS84")
			} else {
				crs <- as.character(NA)
			}
		}
		projection(x) <- crs
		return(x)
	}
)



setMethod('raster', signature(x='asc'), 
	function(x, crs) {
		x <- as(x, 'RasterLayer')
		if (missing(crs)) {
			e <- x@extent
			if (e@xmin > -360.1 & e@xmax < 360.1 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs <- CRS("+proj=longlat +datum=WGS84")
			} else {
				crs <- CRS(as.character(NA))
			}
		}
		projection(x) <- crs
		return(x)
	}
)

	
setMethod('raster', signature(x='kde'), 
	function(x, crs) {
		x <- as(x, 'RasterLayer')
		if (missing(crs)) {
			e <- x@extent
			if (e@xmin > -360.1 & e@xmax < 360.1 & e@ymin > -90.1 & e@ymax < 90.1) { 
				crs <- CRS("+proj=longlat +datum=WGS84")
			} else {
				crs <- CRS(as.character(NA))
			}
		}
		projection(x) <- crs
		return(x)
	}
)




setMethod('raster', signature(x='grf'), 
	function(x, i=1) {
		i <- max(1, i[1])
		if (i != 1) {
			nc <- NCOL(x$data)
			if (i <= nc) {
				x$data <- x$data[,i]
			} else {
				stop('i is higher than the number of simulations in x')
			}
		}
		as(x, 'RasterLayer')
	}
)



setMethod('raster', signature(x='GridTopology'),
	# contributed by Michael Sumner
	function(x) {
		raster(extent(x), nrows=x@cells.dim[2], ncols=x@cells.dim[1])
	}
)

setMethod('raster', signature(x='SpatRaster'),
	function(x) {
		as(x[[1]], "Raster")
	}
)
