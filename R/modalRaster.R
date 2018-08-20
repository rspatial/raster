# Author: Robert J. Hijmans 
# Date :  October 2008
# revised: October 2011
# Version 1.0
# Licence GPL v3


setMethod("modal", signature(x='Raster'),
	function(x, ..., ties='random', na.rm=FALSE, freq=FALSE){

		dots <- list(...)
		if (length(dots) > 0) {
			x <- stack(.makeRasterList(x, ...))
			add <- .addArgs(...)
		} else {
			add <- NULL
		}
		
		nl <- nlayers(x)
		if (nl < 2) {
			stop('there is not much point in computing a modal value for a single layer')
		} else if (nl == 2) {
			warning('running modal with only two layers!')
		}
		
		out <- raster(x)
		
		if (canProcessInMemory(x)) {
			x <- cbind(getValues(x), add)
			x <- setValues(out, apply(x, 1, modal, ties=ties, na.rm=na.rm, freq=freq))
			return(x)
		}

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='modal')
		out <- writeStart(out, filename="")
		for (i in 1:tr$n) {
			v <- cbind( getValues( x, row=tr$row[i], nrows=tr$nrows[i] ), add)
			v <- apply(v, 1, modal, ties=ties, na.rm=na.rm, freq=freq)
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)
		}
		pbClose(pb)
		writeStop(out)
	}
)

