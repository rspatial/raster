# Author: Robert J. Hijmans
# Date : March 2011
# Version 1.0
# Licence GPL v3


if (!isGeneric("atan2")) {
	setGeneric("atan2", function(y, x)
		standardGeneric("atan2"))
}	

setMethod("atan2", signature(y='Raster', x='Raster'),
	function(y, x) { 
	
		compareRaster(x, y)

		ny <- nlayers(y)
		nx <- nlayers(x)
		nl <- max(ny, nx)
		if (nl > 1) {
			r <- brick(x, values=FALSE, nl=nl)
		} else {
			r <- raster(x)
		}
		
		if (canProcessInMemory(r, 3)) {
			r <- setValues(r, atan2(getValues(y), getValues(x)))
		} else {
			tr <- blockSize(x)
			pb <- pbCreate(tr$n)
			r <- writeStart(r, filename=rasterTmpFile())
			for (i in 1:tr$n) {
				v <- atan2(getValues(y, row=tr$row[i], nrows=tr$nrows[i]), getValues(x, row=tr$row[i], nrows=tr$nrows[i]) ) 
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 
			}
			r <- writeStop(r)
			pbClose(pb)
		}
		return(r)
	}
)

