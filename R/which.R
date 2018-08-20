# Author: Robert J. Hijmans
# Date: November 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("Which")) {
	setGeneric("Which", function(x, ...)
		standardGeneric("Which"))
}	


setMethod('Which', signature(x='RasterLayer'), 
function(x, cells=FALSE, na.rm=TRUE, ...) {

		
	if (canProcessInMemory(x, 2)){
		if (cells) {
			return(which(as.logical(getValues(x)) == TRUE))
		} else {
			x <- as.logical(x)
			if (na.rm) {
				x[is.na(x)] <- FALSE
			}
			return(x)
		}
		
	} else {
		out <- raster(x)
		if (cells) {
			vv <- vector()
		} else {
			filename <- rasterTmpFile()
			out <- writeStart(out, filename=filename, format=.filetype(), datatype='INT2S', overwrite=TRUE)
		}
		
		tr <- blockSize(out, n=2)
		pb <- pbCreate(tr$n, type=.progress() )	
		for (i in 1:tr$n) {
			v <- as.logical( getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i] ) )
			
			if (cells) {
				offs <- (tr$row[i]-1) * out@ncols
				vv <- c(vv, which(v==TRUE) + offs)
			} else {
				v <- as.logical(v)
				if (na.rm) {
					v[is.na(v)] <- 0
				}
				out <- writeValues(out, v, tr$row[i])
			}
			pbStep(pb, i)
		}
		pbClose(pb)
		
		
		if (cells) { 
			return(vv)
		} else { 
			out <- writeStop(out)
			return(out) 
		}
	}
}
)

