# Author: Robert J. Hijmans
# Date :  October 2008
# revised: October 2011
# Version 1.0
# Licence GPL v3


setMethod(".median", signature(x='Raster'),
	function(x, na.rm=FALSE, ...){
		
		dots <- list(...)
		if (length(dots) > 0) {
			x <- stack(.makeRasterList(x, ...))
			add <- unlist(.addArgs(...))
		} else {
			add <- NULL
		}
		out <- raster(x)
		d <- dim(x)
		nc <- ncell(out)
		if (is.null(add)) {
			if (nlayers(x) == 1) {
				return(.deepCopyRasterLayer(x))
			}
				
			if (canProcessInMemory(x)) {
				x <- getValues(x)
				x <- setValues(out, apply(x, 1, median, na.rm=na.rm))
				return(x)
			}

			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='median')
			out <- writeStart(out, filename="")
			x <- readStart(x, ...)
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				v <- apply(v, 1, median, na.rm=na.rm)
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			}
			pbClose(pb)
			x <- readStop(x)
			return( writeStop(out) )
		} else {
			d3 <- d[3] + length(add)
			if (canProcessInMemory(x)) {
				if (length(add) == 1) {
					x <- cbind(getValues(x), add)
				} else {
					x <- getValues(x)
					x <- t(apply(x, 1, function(i) c(i, add)))
				}
				x <- setValues(out, apply(x, 1, median, na.rm=na.rm))
				return(x)
			}

			tr <- blockSize(x)
			pb <- pbCreate(tr$n, label='median')
			out <- writeStart(out, filename="")
			x <- readStart(x, ...)
			for (i in 1:tr$n) {
				v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				v <- t(apply(v, 1, function(i) c(i, add)))
				v <- apply(v, 1, median, na.rm=na.rm)
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			}
			pbClose(pb)
			x <- readStop(x)
			return( writeStop(out) )
			
		}
	}
)


