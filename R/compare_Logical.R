# Authors: Robert J. Hijmans 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


.getAdjustedE <- function(r, tr, i, e) {
	startcell <- cellFromRowCol(r, tr$row[i] , 1)
	len <- cellFromRowCol(r, tr$row[i] + (tr$nrows[i]-1), ncol(r)) - startcell + 1
	n <- (startcell / length(e)) %% 1
	if (n > 0 ) {
		start <- round(n * length(e))
	} else {
		start <- 1
	}
	out <- c(e[start:length(e)], rep(e, floor(len/length(e))))
	out[1:len]
}



.asLogical <- function(x) {
	x[x!=0] <- 1
	return(x)
}



setMethod('==', signature(e1='BasicRaster', e2='BasicRaster'),
	function(e1,e2){
		cond <- compareRaster(c(e1, e2), extent=TRUE, rowcol=TRUE, crs=TRUE, tolerance=0.05, stopiffalse=FALSE) 
		return(cond)
	}
)	


setMethod('!=', signature(e1='BasicRaster', e2='BasicRaster'),
	function(e1,e2){
		cond <- compareRaster(c(e1, e2), extent=TRUE, rowcol=TRUE, crs=TRUE, tolerance=0.05, stopiffalse=FALSE) 
		return(!cond)
	}
)	




setMethod('!', signature(x='Raster'),
	function(x){
		if (nlayers(x) > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}

		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			return(setValues(r, ! getValues(x)))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n)			
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- ! .asLogical(getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)		
		}
	}
)	



setMethod("Compare", signature(e1='Raster', e2='logical'),
	function(e1,e2){
		nl <- nlayers(e1)
		if (nl > 1) {
			r <- brick(e1, values=FALSE)
		} else {
			r <- raster(e1)
		}

		
		if (length(e2) > 1 & nl > 1) {
			if (length(e2) != nl) {
				a <- rep(NA, nl)
				a[] <- e2
				e2 <- a
			}

			if (canProcessInMemory(r, 3)) {
				dataType(r) <- 'LOG1S'
				r <- setValues(r, values=t(methods::callGeneric(t(getValues(e1)), e2 ) )	)
			} else {
				tr <- blockSize(r)
				pb <- pbCreate(tr$n)
				r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
				for (i in 1:tr$n) {
					v <- t(methods::callGeneric( t(getValues(e1, row=tr$row[i], nrows=tr$nrows[i])), e2))
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 
				}
				r <- writeStop(r)
				pbClose(pb)
			}
			
		} else {	
		
			if (canProcessInMemory(r, 3)) {
				dataType(r) <- 'LOG1S'
				if (length(e2) > ncell(r)) {
					e2 <- e2[1:ncell(r)]
				}
				r <- setValues(r, values=methods::callGeneric(getValues(e1), e2 ) )			
			} else {
				tr <- blockSize(r)
				pb <- pbCreate(tr$n)
				r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )

				if (length(e2) > 0) {
					for (i in 1:tr$n) {
						e <- .getAdjustedE(r, tr, i, e2)
						v <- methods::callGeneric(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e)
						r <- writeValues(r, v, tr$row[i])
						pbStep(pb, i) 
					}
				} else {
					for (i in 1:tr$n) {
						v <- methods::callGeneric(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e2)
						r <- writeValues(r, v, tr$row[i])
						pbStep(pb, i)
					}
				}
				r <- writeStop(r)
				pbClose(pb)
			}
		}
		return(r)
	}
)



setMethod("Compare", signature(e1='logical', e2='Raster'),
	function(e1,e2){

		nl <- nlayers(e2)
		if (nl > 1) {
			r <- brick(e2, values=FALSE)
		} else {
			r <- raster(e2)
		}

		
		if (length(e1) > 1 & nl > 1) {
			if (length(e1) != nl) {
				a <- rep(NA, nl)
				a[] <- e1
				e1 <- a
			}

			if (canProcessInMemory(r, 3)) {
				dataType(r) <- 'LOG1S'
				r <- setValues(r, values=t(methods::callGeneric(e1, t(getValues(e2)) ) )	)
			} else {
				tr <- blockSize(r)
				pb <- pbCreate(tr$n)
				r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
				for (i in 1:tr$n) {
					v <- t(methods::callGeneric(e1,  t(getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))))
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 
				}
				r <- writeStop(r)
				pbClose(pb)
			}
			
		} else {	
		
			if (canProcessInMemory(r, 3)) {
				dataType(r) <- 'LOG1S'
				if (length(e1) > ncell(r)) {
					e1 <- e1[1:ncell(r)]
				}
				r <- setValues(r, values=methods::callGeneric(e1, getValues(e2) ) )			
			} else {
				tr <- blockSize(r)
				pb <- pbCreate(tr$n)
				r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )

				if (length(e1) > 0) {
					for (i in 1:tr$n) {
						e <- .getAdjustedE(r, tr, i, e1)
						v <- methods::callGeneric(e1, getValues(e, row=tr$row[i], nrows=tr$nrows[i]))
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
				pbClose(pb)
			}
		}
		return(r)
	}

)




setMethod("Compare", signature(e1='Raster', e2='numeric'),
	function(e1, e2){

		nl <- nlayers(e1)

		if (nl > 1) {
			r <- brick(e1, values=FALSE)
		} else {
			r <- raster(e1)
		}
		
		if (length(e2) > 1 & nl > 1) {
			if (length(e2) != nl) {
				a <- rep(NA, nl)
				a[] <- e2
				e2 <- a
			}

			if (canProcessInMemory(r, 3)) {
				dataType(r) <- 'LOG1S'
				r <- setValues(r, values=t(methods::callGeneric(t(getValues(e1)), e2 ) )	)
			} else {
				tr <- blockSize(r)
				pb <- pbCreate(tr$n)
				r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
				for (i in 1:tr$n) {
					v <- t(methods::callGeneric( t(getValues(e1, row=tr$row[i], nrows=tr$nrows[i])), e2))
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 
				}
				r <- writeStop(r)
				pbClose(pb)
			}
			
		} else {	
		
			if (canProcessInMemory(r, 3)) {
				dataType(r) <- 'LOG1S'
				if (length(e2) > ncell(r)) {
					e2 <- e2[1:ncell(r)]
				}
				r <- setValues(r, values=methods::callGeneric(getValues(e1), e2))
			} else {
				tr <- blockSize(r)
				pb <- pbCreate(tr$n)
				r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
				
				if (length(e2) > 0) {
					for (i in 1:tr$n) {
						e <- .getAdjustedE(r, tr, i, e2)
						v <- methods::callGeneric(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e)
						r <- writeValues(r, v, tr$row[i])
						pbStep(pb, i) 
					}
				} else {
					for (i in 1:tr$n) {
						v <- methods::callGeneric(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), e2)
						r <- writeValues(r, v, tr$row[i])
						pbStep(pb, i)
					}
				}
				
				r <- writeStop(r)
				pbClose(pb)
			}
		}

		return(r)
		
	}
)	





setMethod("Compare", signature(e1='numeric', e2='Raster'),
	function(e1, e2){

		nl <- nlayers(e2)

		if (nl > 1) {
			r <- brick(e2, values=FALSE)
		} else {
			r <- raster(e2)
		}
		
		if (length(e1) > 1 & nl > 1) {
			if (length(e1) != nl) {
				a <- rep(NA, nl)
				a[] <- e1
				e1 <- a
			}

			if (canProcessInMemory(r, 3)) {
				dataType(r) <- 'LOG1S'
				r <- setValues(r, values=t(methods::callGeneric(e1, t(getValues(e2))) )	)
			} else {
				tr <- blockSize(r)
				pb <- pbCreate(tr$n)
				r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
				for (i in 1:tr$n) {
					v <- t(methods::callGeneric(e1, t(getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))))
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 
				}
				r <- writeStop(r)
				pbClose(pb)
			}
			
		} else {	
		
			if (canProcessInMemory(r, 3)) {
				dataType(r) <- 'LOG1S'
				if (length(e1) > ncell(r)) {
					e1 <- e1[1:ncell(r)]
				}
				r <- setValues(r, values=methods::callGeneric(e1, getValues(e2)))
			} else {
				tr <- blockSize(r)
				pb <- pbCreate(tr$n)
				r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
				
				if (length(e2) > 0) {
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
				pbClose(pb)
			}
		}

		return(r)
		
	}
)	




setMethod("Compare", signature(e1='Raster', e2='Raster'),
    function(e1, e2){ 
	
		if (nlayers(e1) > 1) {
			if (nlayers(e2) > 1 & nlayers(e2) != nlayers(e1)) {
				stop('number of layers of objects do not match')
			}
			r <- brick(e1, values=FALSE)
		} else if (nlayers(e2) > 1) {
			r <- brick(e2, values=FALSE)
		} else {
			r <- raster(e1)
		}
	
		cond <- compareRaster(c(r, e2), extent=TRUE, rowcol=TRUE, crs=TRUE, tolerance=0.05, stopiffalse=FALSE) 
		if (!cond) {
			stop("Cannot compare Rasters that have different BasicRaster attributes. See compare()")
		}	
		
		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, methods::callGeneric(getValues(e1), getValues(e2)))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n)
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- methods::callGeneric(getValues(e1, row=tr$row[i], nrows=tr$nrows[i]), getValues(e2, row=tr$row[i], nrows=tr$nrows[i]))
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}	
		return(r)
	}
)




setMethod("Logic", signature(e1='Raster', e2='Raster'),
    function(e1, e2){ 
	
		if (nlayers(e1) > 1) {
			r <- brick(e1, values=FALSE)
			if (nlayers(e2) > 1 & nlayers(e2) != nlayers(e1)) {
				stop('number of layers of objects do not match')
			}
		} else if (nlayers(e2) > 1) {
			r <- brick(e2, values=FALSE)
		} else {
			r <- raster(e1)
		}
	
		cond <- compareRaster(c(r, e2), extent=TRUE, rowcol=TRUE, crs=TRUE, tolerance=0.05, stopiffalse=FALSE) 
		if (!cond) {
			stop("Cannot compare Rasters that have different BasicRaster attributes. See compare()")
		}	
		
		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			r <- setValues(r, methods::callGeneric(.asLogical(getValues(e1)), .asLogical(getValues(e2))))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n)
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- methods::callGeneric(.asLogical(getValues(e1, row=tr$row[i], nrows=tr$nrows[i])), .asLogical(getValues(e2, row=tr$row[i], nrows=tr$nrows[i])))
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}	
		return(r)
	}
)


setMethod("Compare", signature(e1='Extent', e2='Extent'),
	function(e1,e2){
		a <- methods::callGeneric(e2@xmin, e1@xmin)
		b <- methods::callGeneric(e1@xmax, e2@xmax)
		c <- methods::callGeneric(e2@ymin, e1@ymin)
		d <- methods::callGeneric(e1@ymax, e2@ymax)
		a & b & c & d
	}
)	

