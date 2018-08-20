# Authors: Robert J. Hijmans 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("is.na", signature(x='Raster'),
	function(x) {
		if (nlayers(x) > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}
		
		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			return( setValues(r, is.na(getValues(x))) )
		} else {
			tr <- blockSize(x)
			
			pb <- pbCreate(tr$n, label='is.na')			
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- is.na( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]) )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)

			return(r)
		}
	}
)	



setMethod("is.nan", signature(x='Raster'),
	function(x) {
		if (nlayers(x) > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}

		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			return( setValues(r, is.nan(getValues(x))) )
		} else {
			tr <- blockSize(x)
			
			pb <- pbCreate(tr$n, label='is.na')			
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- is.nan( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]) )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)

			return(r)
		}
	}
)	




setMethod("is.finite", signature(x='Raster'),
	function(x) {
		if (nlayers(x) > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}
		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			return( setValues(r, is.finite(getValues(x))) )
		} else {
			tr <- blockSize(x)
			
			pb <- pbCreate(tr$n, label='is.na')			
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- is.finite( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]) )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)

			return(r)
		}
	}
)	




setMethod("is.infinite", signature(x='Raster'),
	function(x) {
		if (nlayers(x) > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}

		if (canProcessInMemory(r, 3)) {
			dataType(r) <- 'LOG1S'
			return( setValues(r, is.infinite(getValues(x))) )
		} else {
			tr <- blockSize(x)
			
			pb <- pbCreate(tr$n, label='is.na')			
			r <- writeStart(r, filename=rasterTmpFile(), datatype='LOG1S', format=.filetype(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- is.infinite( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i]) )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)

			return(r)
		}
	}
)	

