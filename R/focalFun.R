# Author: Robert J. Hijmans
# Date : March 2014
# Version 1.0
# Licence GPL v3


#if ( !isGeneric("focalFun") ) {
#	setGeneric("focalFun", function(x, ...)
#		standardGeneric("focalFun"))
#}


#setMethod('focalFun', signature(x='Raster'), 
.focalFun <- function(x, fun, ngb=5, filename='', ...) {
		
	out <- raster(x)
	
	if (.doCluster()) {
		cl <- getCluster()
		on.exit( returnCluster() )
		
		if (canProcessInMemory(x)) {
			v <- getValuesFocal(x, 1, nrow(x), ngb=ngb, array=TRUE)
			v <- parallel::parApply(cl, v, 1, fun)
			out <- setValues(out, v)
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
			return(out)
			
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='focalFun', ...)
			out <- writeStart(out, filename=filename, ...)
			for (i in 1:tr$n) {
				v <- getValuesFocal(x, tr$row[i], tr$nrows[i], ngb=ngb, array=TRUE)
				v <- parallel::parApply(cl, v, 1, fun)
				out <- writeValues(out, v, tr$row[i])
			}
		}
		return(writeStop(out))
	} else {
	

		if (canProcessInMemory(x)) {
			v <- getValuesFocal(x, 1, nrow(x), ngb=ngb, array=TRUE)
			v <- apply(v, 1, fun)
			out <- setValues(out, v)
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
			return(out)
			
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='focalFun', ...)
			out <- writeStart(out, filename=filename, ...)
			for (i in 1:tr$n) {
				v <- getValuesFocal(x, tr$row[i], tr$nrows[i], ngb=ngb, array=TRUE)
				v <- apply(v, 1, fun)
				out <- writeValues(out, v, tr$row[i])
			}
		}
		return(writeStop(out))
	}
}	
	
#)


