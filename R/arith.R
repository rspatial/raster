# Author: Robert J. Hijmans
# Date :  January 2009
# Version 1.0
# Licence GPL v3


setMethod("Arith", signature(e1='Raster', e2='missing'),
    function(e1, e2){ 
		methods::callGeneric(0, e1)
	}
)


setMethod("Arith", signature(e1='Raster', e2='Raster'),
    function(e1, e2){ 

		if (!hasValues(e1)) { stop('first Raster object has no values') }
		if (!hasValues(e2)) { stop('second Raster object has no values') }
		
		nl1 <- nlayers(e1)
		nl2 <- nlayers(e2)
		nl <- max(nl1, nl2)

		proj1 <-.getCRS(e1)
		proj2 <-.getCRS(e2)
	
		if ( ! compareRaster(e1, e2, crs=FALSE, stopiffalse=FALSE) ) {
			if ( compareRaster(e1, e2, extent=FALSE, rowcol=FALSE, crs=TRUE, res=TRUE, orig=TRUE, stopiffalse=TRUE) ) {
				ie <- intersect(extent(e1), extent(e2))
				if (is.null(ie)) { 	stop() }
				warning('Raster objects have different extents. Result for their intersection is returned')
				e1 <- crop(e1, ie)
				e2 <- crop(e2, ie)
			} else {
				stop()  # stops anyway because compareRaster returned FALSE
			}
		}

		if (nl > 1) {
			r <- brick(e1, values=FALSE, nl=nl)
		} else {
			r <- raster(e1)
		}

		
		if (canProcessInMemory(r, 4 * nlayers(e2))) {
			if (nl1 == nl2 ) {
				return( setValues(r, values=methods::callGeneric( getValues(e1), getValues(e2))) )
			} else {
				return( setValues(r, matrix(methods::callGeneric( as.vector(getValues(e1)), as.vector(getValues(e2))), ncol=nl)) )
			}
			
		} else {
		
			tr <- blockSize(e1)
			pb <- pbCreate(tr$n, label='arith')
			e1 <- readStart(e1)
			e2 <- readStart(e2)
			r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
			if (nl1 == nl2 ) {
				for (i in 1:tr$n) {
					v1 <- getValues(e1, row=tr$row[i], nrows=tr$nrows[i])
					v2 <- getValues(e2, row=tr$row[i], nrows=tr$nrows[i])
					v <- methods::callGeneric( v1, v2 )
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 	
				}
			} else {
				for (i in 1:tr$n) {
					v1 <- as.vector(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]))
					v2 <- as.vector(getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))
					v <- matrix(methods::callGeneric( v1, v2 ), ncol=nl)
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 	
				}
			}
			r <- writeStop(r)
			e1 <- readStop(e1)
			e2 <- readStop(e2)
			pbClose(pb)
			return(r)
			
		}
	}	
)




setMethod("Arith", signature(e1='RasterLayer', e2='numeric'),
    function(e1, e2){ 
		if (!hasValues(e1)) { stop('RasterLayer has no values') }

		r <- raster(e1)
		names(r) <- names(e1)
		if (canProcessInMemory(e1, 4)) {
			if (length(e2) > ncell(r)) {
				e2 <- e2[1:ncell(r)]
			}
			return ( setValues(r,  methods::callGeneric(as.numeric(getValues(e1)), e2) ) )
			
		} else {
			tr <- blockSize(e1)
			pb <- pbCreate(tr$n, label='arith')			
			r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
			e1 <- readStart(e1)

			if (length(e2) > 0) {
				for (i in 1:tr$n) {
					e <- .getAdjustedE(r, tr, i, e2)
					v <- methods::callGeneric(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e)
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 	
				}
			} else {
				for (i in 1:tr$n) {
					v <- methods::callGeneric( getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e2 )
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i)
				}
			}
			r <- writeStop(r)
			e1 <- readStop(e1)
			pbClose(pb)
			return(r)
		}		
	}
)



setMethod("Arith", signature(e1='numeric', e2='RasterLayer'),
    function(e1, e2){ 
		stopifnot(hasValues(e2))

		r <- raster(e2)
		names(r) <- names(e2)
		if (canProcessInMemory(e2, 4)) {
			if (length(e1) > ncell(r)) {
				e1 <- e1[1:ncell(r)]
			}
			return ( setValues(r,  methods::callGeneric(e1, getValues(e2)) ) )
			
		} else {
			tr <- blockSize(e2)
			pb <- pbCreate(tr$n, label='arith')			
			r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
			e2 <- readStart(e2)

			if (length(e1) > 0) {
				for (i in 1:tr$n) {
					e <- .getAdjustedE(r, tr, i, e1)
					v <- methods::callGeneric(e, getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 	
				}
			} else {
				for (i in 1:tr$n) {
					v <- methods::callGeneric(e1, getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i)
				}
			}
			r <- writeStop(r)
			e2 <- readStop(e2)
			
			pbClose(pb)
			return(r)
		}		
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


setMethod("Arith", signature(e1='RasterLayer', e2='logical'),
    function(e1, e2){ 
		e2 <- as.integer(e2)
		methods::callGeneric(e1, e2)
	}
)

setMethod("Arith", signature(e1='logical', e2='RasterLayer'),
    function(e1, e2){ 
		e1 <- as.integer(e1)
		methods::callGeneric(e1, e2)
	}
)



setMethod("Arith", signature(e1='RasterStackBrick', e2='numeric'),
    function(e1, e2) {
	
		if (length(e2) > 1) {
			nl <- nlayers(e1)
			if (length(e2) != nl) {
				a <- rep(NA, nl)
				a[] <- e2
				e2 <- a
			}

			b <- brick(e1, values=FALSE)
			names(b) <- names(e1)
			
			if (canProcessInMemory(e1, 4)) {
				return( setValues(b, t(methods::callGeneric( t(getValues(e1)), e2))) )
			}
			
			tr <- blockSize(b)
			pb <- pbCreate(tr$n, label='arith')
			b <- writeStart(b, filename=rasterTmpFile(), bandorder='BIL')
			e1 <- readStart(e1)

			for (i in 1:tr$n) {
				v <- t (methods::callGeneric( t(getValues(e1, row=tr$row[i], nrows=tr$nrows[i])), e2) )
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i)
			}
			b <- writeStop(b)
			e1 <- readStop(e1)
			pbClose(pb)
			return(b)
		}
		
		# else:

		b <- brick(e1, values=FALSE)
		names(b) <- names(e1)
		
		if (canProcessInMemory(e1, 4)) {
			return ( setValues(b,  methods::callGeneric(getValues(e1), e2) ) )
		} else {
			tr <- blockSize(b)
			pb <- pbCreate(tr$n, label='arith')
			b <- writeStart(b, filename=rasterTmpFile())
			e1 <- readStart(e1)

			for (i in 1:tr$n) {
				v <- methods::callGeneric( getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e2)
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i)
			}
			b <- writeStop(b)
			e1 <- readStop(e1)
			pbClose(pb)
			return(b)
		}
	}
)



setMethod("Arith", signature(e1='numeric', e2='RasterStackBrick'),
    function(e1, e2) {
	
		if (length(e1) > 1) {
			nl <- nlayers(e2)
			if (length(e1) != nl) {
				a <- rep(NA, nl)
				a[] <- e1
				e1 <- a
			}
					

			b <- brick(e2, values=FALSE)
			names(b) <- names(e2)
			
			if (canProcessInMemory(e2, 4)) {
				return( setValues(b, t(methods::callGeneric( e1, t(getValues(e2))))) )
			}
			
			tr <- blockSize(b)
			pb <- pbCreate(tr$n, label='arith')
			e2 <- readStart(e2)

			b <- writeStart(b, filename=rasterTmpFile())
			for (i in 1:tr$n) {
				v <- t (methods::callGeneric( e1, t(getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))) )
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i)
			}
			b <- writeStop(b)
			e2 <- readStop(e2)
			pbClose(pb)
			return(b)
		}
		
		# else:

		b <- brick(e2, values=FALSE)
		names(b) <- names(e2)
		
		if (canProcessInMemory(e2, 4)) {
			return ( setValues(b,  methods::callGeneric(e1, getValues(e2)) ) )
		} else {
	
			tr <- blockSize(b)
			pb <- pbCreate(tr$n, label='arith')
			b <- writeStart(b, filename=rasterTmpFile())
			e2 <- readStart(e2)
			for (i in 1:tr$n) {
				v <- methods::callGeneric( e1, getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i)
			}
			b <- writeStop(b)
			e2 <- readStop(e2)
			pbClose(pb)
			return(b)
		}
	}
)


setMethod("Arith", signature(e1='RasterStackBrick', e2='logical'),  # for Arith with NA
    function(e1, e2){ 
		e2 <- as.integer(e2)
		methods::callGeneric(e1, e2)
	}
)

setMethod("Arith", signature(e1='logical', e2='RasterStackBrick'),
    function(e1, e2){ 
		e1 <- as.integer(e1)
		methods::callGeneric(e1, e2)
	}
)


.getE2 <- function(e2) {
	n <- length(e2)
	if (n == 1) {
		e2 <- rep(e2, 4)
	} else if (n == 2) {
		e2 <- rep(e2, each=2)
	} else if (n != 4) {
		stop('use 1, 2, or 4 numbers in arithmetic operations with an Extent')
	} 
	e2
}


.multiply_Extent <- function(e1, e2) {	
	
	e2 <- abs(e2)
	
	if (length(e2) == 4) {
		return(extent(as.vector(e1) * e2))
	}
	e2 <- rep_len(e2, length.out=2)

	rx <- e1@xmax - e1@xmin
	ry <- e1@ymax - e1@ymin
	dx <- (rx * e2[1] - rx) / 2
	dy <- (ry * e2[2] - ry) / 2

	e1@xmax <- e1@xmax + dx
	e1@xmin <- e1@xmin - dx
	e1@ymax <- e1@ymax + dy
	e1@ymin <- e1@ymin - dy
	
	return(e1)
}
		
		
.add_Extent <- function(e1, e2, g) {	
	
	if (length(e2) == 4) {
		return(extent(as.vector(e1) + e2))
	}
	e2 <- rep_len(e2, length.out=2)
	dx <- e2[1] / 2
	dy <- e2[2] / 2
	e1@xmax <- e1@xmax + dx
	e1@xmin <- e1@xmin - dx
	e1@ymax <- e1@ymax + dy
	e1@ymin <- e1@ymin - dy
	
	return(e1)
}


setMethod("Arith", signature(e1='Extent', e2='numeric'),
	function(e1, e2){ 
		g <- as.vector(.Generic) 
		if (g %in% c("/", "*")) {
			if (g == '/') {
				e2 <- 1 / e2
			}
			return( .multiply_Extent(e1, e2) )
		} else if (g %in% c("+", "-")) {
			if (g == '-') {
				e2 <- -1 * e2
			}
			return( .add_Extent(e1, e2) )
		}
		extent(methods::callGeneric(as.vector(e1), .getE2(e2)))
	}
)

setMethod("Arith", signature(e1='numeric', e2='Extent'),
    function(e1, e2){ 
		methods::callGeneric(e2,e1)
	}
)

		