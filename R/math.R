# Authors: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3


setMethod("Math", signature(x='Raster'),
    function(x){ 

		if (!hasValues(x)) {
			return(x)
		}
		#funname <- as.character(sys.call(sys.parent())[[1]])
		funname <- .Generic
		
		nl <- nlayers(x)
		if (nl > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}

		if (substr(funname, 1, 3) == 'cum' ) { 
			if (nl == 1) {
				if (canProcessInMemory(r, 3)) {
					r <- setValues(r, do.call(funname, list(values(x))))
				} else {
					tr <- blockSize(x)
					pb <- pbCreate(tr$n, label='math')			
					r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
					x <- readStart(x)
					last <- 0
					for (i in 1:tr$n) {
						v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
						if (i==1) {
							v <- do.call(funname, list(v))
						} else {
							v <- do.call(funname, list(c(last, v)))[-1]
						} 
						last <- v[length(v)]
						r <- writeValues(r, v, tr$row[i])
						pbStep(pb, i) 
					}
					r <- writeStop(r)
					x <- readStop(x)
					pbClose(pb)
				}
				return(r)
			}
			
			if (canProcessInMemory(r, 3)) {
				r <- setValues(r, t( apply(getValues(x), 1, funname)) )
			} else {
			
				tr <- blockSize(x)
				pb <- pbCreate(tr$n, label='math')
				r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
				x <- readStart(x)

				for (i in 1:tr$n) {
					v <- t( apply(getValues(x, row=tr$row[i], nrows=tr$nrows[i]), 1, funname) )
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 
				}
				r <- writeStop(r)
				x <- readStop(x)
				pbClose(pb)
			}
			
		} else {
		
			if (canProcessInMemory(r, 3)) {
				r <- setValues(r, methods::callGeneric(getValues(x)))
			} else {
				if (funname %in% c('floor', 'ceiling', 'trunc')) {
					datatype <- 'INT4S'
				} else {
					datatype <- .datatype()
				}
			
				tr <- blockSize(x)
				pb <- pbCreate(tr$n, label='math')
				r <- writeStart(r, filename=rasterTmpFile(), datatype=datatype, overwrite=TRUE )
				x <- readStart(x)

				for (i in 1:tr$n) {
					v <- methods::callGeneric( getValues(x, row=tr$row[i], nrows=tr$nrows[i]) )
					r <- writeValues(r, v, tr$row[i])
					pbStep(pb, i) 
				}
				r <- writeStop(r)
				x <- readStop(x)
				pbClose(pb)
			}
		}
		return(r)
	}
)




setMethod("Math2", signature(x='Raster'), 
	function (x, digits=0) {
		
		digits <- round(digits)
		
		if (nlayers(x) > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}

		if (canProcessInMemory(r, 3)) {
			r <- setValues(r, methods::callGeneric( getValues(x), digits))
		} else {
			if (digits <= 0) {
				datatype <- 'INT4S'
			} else {
				datatype <- .datatype()
			}

			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='math')
			r <- writeStart(r, filename=rasterTmpFile(), datatype=datatype, format=.filetype(), overwrite=TRUE )
			x <- readStart(x)
		
			for (i in 1:tr$n) {
				v <- methods::callGeneric( getValues(x, row=tr$row[i], nrows=tr$nrows[i]), digits )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			x <- readStop(x)
			pbClose(pb)
		}
		return(r)
	}
)


if (!isGeneric("log")) {
	setGeneric("log", function(x, ...)
		standardGeneric("log"))
}	


setMethod("log", signature(x='Raster'),
    function(x, base=exp(1)){ 
	
		nl <- nlayers(x)
		if (nl > 1) {
			r <- brick(x, values=FALSE)
		} else {
			r <- raster(x)
		}

		if (canProcessInMemory(r, 3)) {
			r <- setValues(r, log(values(x), base=base))
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='math')
			r <- writeStart(r, '', overwrite=TRUE )
			x <- readStart(x)
			
			for (i in 1:tr$n) {
				v <- log( getValues(x, row=tr$row[i], nrows=tr$nrows[i]), base=base )
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			x <- readStop(x)
			
			pbClose(pb)
		}
		return(r)
	}
)

