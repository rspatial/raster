# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3



setMethod('cut', signature(x='Raster'), 
function(x, breaks, ..., filename='', format, datatype='INT2S', overwrite, progress)  {
	
	if (! hasValues(x) ) { 
		warning('x has no values, nothing to do')
		return(x) 
	}
	
	filename <- trim(filename)
	if (missing(format)) { format <- .filetype(format=format, filename=filename) } 
	if (missing(overwrite)) { overwrite <- .overwrite()	}
	if (missing(progress)) { progress <- .progress() }

	nl <- nlayers(x)
	if (nl == 1) { out <- raster(x)
	} else { out <- brick(x, values=FALSE) }	
	
	if (canProcessInMemory(out, n=nl*2 + 2)) {

		if (nl > 1) {
			values(out) <- apply(getValues(x), 2, function(x) as.numeric(cut(x, breaks=breaks, labels=FALSE, ...)))
		} else {
			values(out) <- as.numeric(cut(getValues(x), breaks=breaks, labels=FALSE, ...))
		}
		if ( filename != "" ) { 
			out <- writeRaster(out, filename=filename, format=format, datatype=datatype, overwrite=overwrite, progress=progress )
		}
		return(out)
				
	} else {

		if (filename == '') { filename <- rasterTmpFile() }

		if (length(breaks) == 1) {
			breaks <- round(breaks)
			stopifnot(breaks > 1)
			probs <- c(0, 1:breaks * 1/breaks)
			breaks <- stats::na.omit(sampleRegular(x, 10000, useGDAL=TRUE))
			warning('breaks are approximate, based on a sample of ', length(breaks), ' cells that are not NA')
			breaks <- stats::quantile(breaks, probs, names=FALSE)
			breaks[1] <- -Inf
			breaks[length(breaks)] <- Inf
		}
		
		out <- writeStart(out, filename=filename, format=format, datatype=datatype, overwrite=overwrite, progress=progress )
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, progress=progress, label='cut')

		if (nl > 1) {
			for (i in 1:tr$n) {
				res <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				res <- apply(res, 2, function(x) as.numeric(cut(x, breaks=breaks, labels=FALSE, ...)))
				out <- writeValues(out, res, tr$row[i])
				pbStep(pb, i)
			}
		} else {
			for (i in 1:tr$n) {
				res <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
				res <- as.numeric(cut(res, breaks=breaks, labels=FALSE, ...))
				out <- writeValues(out, res, tr$row[i])
				pbStep(pb, i)
			}
		}
		
		out <- writeStop(out)
		pbClose(pb)
		return(out)
	}
}
)

