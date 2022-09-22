# Author: Robert J. Hijmans
# Date : September 2008
# Version 0.9
# Licence GPL v3


setMethod("stack", signature(x='missing'), 
function(x) {
	return(methods::new("RasterStack"))
	}
)


setMethod("stack", signature(x='Raster'), 
	function(x, ..., layers=NULL) {
		rlist <- list(x, ...)
		if ( length(rlist) == 1 ) {
			if (inherits(x, 'RasterLayer')) {
				stack(rlist)
			} else if (inherits(x, 'RasterBrick')) {
				return( .stackFromBrick(x, bands=layers) )
			} else { # RasterStack
				return(x)
			}
		} else {
			stack( .makeRasterList(rlist) )
		} 
	}
)



setMethod("stack", signature(x='character'), 
function(x, ..., bands=NULL, varname="", native=FALSE, RAT=TRUE, quick=FALSE) {

	if (length(x) == 0) {
		stop("no filenames supplied")
	}
	
	rlist <- c(x, list(...))


    if ( varname != "") {
		if (length(rlist) == 1) {
			return(.stackCDF(x, varname=varname, bands=bands))
		} else {
			s <- stack(sapply(rlist, function(i) stack(i, varname=varname, bands=bands)))
			return(s)
		}
		
	} else {
		
		if (length(rlist) == 1) {
			
			return(.quickStackOneFile(x, bands=bands, native=native))
			
		} else if (quick) {
			if (!is.null(bands)) {
				stop("cannot do 'quick' if bands is not NULL")
			}
			return(.quickStack(rlist, native=native))
		}
		
		return(stack(rlist, bands=bands, native=native, RAT=RAT))
	}
} )



setMethod("stack", signature(x='list'), 
function(x, bands=NULL, native=FALSE, RAT=TRUE, ...) {

	if (inherits(x, 'data.frame')) {
		return(utils::stack(x, ...))
	}

	lstnames <- names(x)
	if (is.null(lstnames)) {
		namesFromList <- FALSE
	} else {
		lstnames <- validNames(lstnames)
		namesFromList <- TRUE
	}

	# first try simplest case, all RasterLayer objects
	cls <- sapply(x, function(i) inherits(i, 'RasterLayer'))
	if (all(cls)) {
		
		hd <- sapply(x, function(i) hasValues(i) )
		if (!all(hd)) {
			if (sum(hd) == 0) {
				s <- methods::new("RasterStack")
				s@nrows <- x[[1]]@nrows
				s@ncols <- x[[1]]@ncols
				s@extent <- x[[1]]@extent
				crs(s) <- x[[1]]@srs
				return(s)
			}
			warning('RasterLayer objects without cell values were removed')
			x <- x[hd]
		}
		if (length(x) > 1) {
			compareRaster(x)
		}
		s <- methods::new("RasterStack")
		s@nrows <- x[[1]]@nrows
		s@ncols <- x[[1]]@ncols
		s@extent <- x[[1]]@extent
		crs(s) <- x[[1]]@srs
		s@layers <- x
		if (namesFromList) {
			names(s) <- lstnames
		} else {
			names(s) <- sapply(x, names)
		}
		return(s)
	}
	
	
	r <- list()

	if (is.character(x[[1]])) {
		first <- raster(x[[1]], native=native, RAT=RAT, ...)
	} else {
		first <- raster(x[[1]])	
	}
	if (!is.null(bands)) {
		lb <- length(bands)
		bands <- bands[bands %in% 1:nbands(first)]
		if (length(bands) == 0) {
			stop('no valid bands supplied')
		}
		if (length(bands) < lb) {
			warning('invalid band numbers ignored')
		}
	} 

	j <- 1
	for (i in seq(along.with=x)) {
		if (is.character(x[[i]])) {
			if (!is.null(bands)) {
				for (b in bands) {
					r[[j]] <- raster(x[[i]], band=b, native=native, RAT=RAT, ...)
					if (namesFromList) {
						names(r[[j]]) <- paste(lstnames[i], '_', b, sep='')
					}
					j <- j + 1
				}
			} else {
				r[[j]] <- raster(x[[i]], band=1, native=native, RAT=RAT, ...)
				bds <- nbands(r[[j]])

				if (namesFromList) {
					if (bds > 1) {
						names(r[[j]]) <- paste(lstnames[i], '_1', sep='')						
					} else {
						names(r[[j]]) <- lstnames[i]
					}
				}
				j <- j + 1
				if (bds > 1) {
					for (b in 2:bds) {
						r[[j]] <- raster(x[[i]], band=b, native=native, RAT=RAT, ...)
							
						if (namesFromList) {
							names(r[[j]]) <- paste(lstnames[i], '_', b, sep='')
						}
						j <- j + 1
					}
				}
			}
		} else if (methods::extends(class(x[[i]]), "Raster")) {
			if (inherits(x[[i]], 'RasterStackBrick')) {
			# commented on 2012/11/21 because bands should 
			# only refer to files, not to layers in Raster objects
			#	if (!is.null(bands)) {
			#		for (b in bands) {
			#			r[j] <- raster(x[[i]], b)
			#			j <- j + 1
			#		}
			#	} else {
					if (inherits(x[[i]], 'RasterBrick')) {
						x[[i]] <- stack(x[[i]])
					}
					r <- c(r, x[[i]]@layers)
					j <- j + nlayers(x[[i]])
			#	}
			} else {
				r[[j]] <- x[[i]]
				if (namesFromList) {
					names(r[[j]]) <- lstnames[i]
				}
				j <- j + 1				
			}
		} else {
			stop("Arguments should be Raster* objects or filenames")	
		}
	}
	
	if ( length(r) == 1 ) {
		r <- r[[1]]
		if ( hasValues(r) ) {
			return( addLayer( methods::new("RasterStack"), r) )
		} else {
			x <- methods::new("RasterStack")
			x@nrows <- r@nrows
			x@ncols <- r@ncols
			x@extent <- r@extent
			crs(x) <- crs(r)
			if(rotated(r)) {
				x@rotated = r@rotated
				x@rotation = r@rotation
			}
			return(x)
		}
	} else {
		return(addLayer(methods::new("RasterStack"), r))
	}
}
)



setMethod("stack", signature(x='SpatialGridDataFrame'), 
	function(x, ...) {
		.stackFromBrick(brick(x), ...)
	}
)

	

setMethod("stack", signature(x='SpatialPixelsDataFrame'), 
	function(x, ...) {
		x <- as(x, 'SpatialGridDataFrame')
		.stackFromBrick(brick(x), ...)
	}
)



setMethod('stack', signature(x='kasc'), 
	function(x) {
		as(x, 'RasterStack')
	}
)

setMethod('stack', signature(x='SpatRaster'), 
	function(x) {
		x <- as(x, "Raster")
		stack(x)
	}
)
