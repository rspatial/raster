# Authors: Robert J. Hijmans
# Date :  May 2012
# Version 1.0
# Licence GPL v3


.range <- function(x, ..., na.rm=FALSE) {

		dots <- list(...)
		if (length(dots) > 0) {
			d <- sapply(dots, function(i) inherits(i, 'Raster'))
			if (any(d)) {
				x <- .makeRasterList(x, dots[d])
				if (length(x) > 1) {
					x <- stack(x)
				} else {
					x <- x[[1]]
				}
			}
			add <- .addArgs(unlist(dots[!d]))
		} else {
			add <- NULL
		}

		if (nlayers(x)==1 & length(add)==0) {
			warning('Cannot compute a range from a single RasterLayer; see cellStats')
			return(x)
		}	
		
		out <- raster(x)
		out <- brick(out, nl=2, values=FALSE)
		names(out) <- c('range_min', 'range_max')
	
		if (canProcessInMemory(x)) {
			
			if (!is.null(add)) {
				add <- range(add, na.rm=na.rm)
				x <- cbind(getValues(x), add[1], add[2])
			} else {
				x <- getValues(x)
			}
			x <- apply(x, 1, range, na.rm=na.rm)
			out <- setValues(out, t(x))
			return(out)
		}
		
		tr <- blockSize(x)
		out <- writeStart(out, filename="")
		pb <- pbCreate(tr$n, label='range',)
		if (!is.null(add)) {
			add <- range(add)
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				v <- cbind(v, add[1], add[2])
				v <- apply(v, 1, FUN=range, na.rm=na.rm)
				out <- writeValues(out, t(v), tr$row[i])
				pbStep(pb, i) 
			} 
		} else {
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				v <- apply(v, 1, FUN=range, na.rm=na.rm)
				out <- writeValues(out, t(v), tr$row[i])
				pbStep(pb, i) 
			} 
		}
		pbClose(pb)			
		out <- writeStop(out)
		names(out) <- c('range_min', 'range_max')		
		out
}



