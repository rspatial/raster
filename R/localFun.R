# Author: Robert J. Hijmans
# Date : February 2014
# Version 1.0
# Licence GPL v3


if ( !isGeneric("localFun") ) {
	setGeneric("localFun", function(x, y, ...)
		standardGeneric("localFun"))
}


setMethod('localFun', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, ngb=5, fun, filename='', ...) {
		
		compareRaster(x,y)
		out <- raster(x)
		nc1 <- 1:(ngb*ngb)
		nc2 <- ((ngb*ngb)+1):(2*(ngb*ngb))
		
		if (canProcessInMemory(x, n=2*ngb)) {
			vx <- getValuesFocal(x, 1, nrow(x), ngb=ngb)
			vy <- getValuesFocal(y, 1, nrow(y), ngb=ngb)
			values(out) <- apply(cbind(vx, vy), 1, function(x, ...) fun(x[nc1], x[nc2], ...))
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
			return(out)
			
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='localFun', ...)
			out <- writeStart(out, filename=filename, ...)
			for (i in 1:tr$n) {
				vx <- getValuesFocal(x, tr$row[i], tr$nrows[i], ngb=ngb)
				vy <- getValuesFocal(y, tr$row[i], tr$nrows[i], ngb=ngb)
				v <- apply(cbind(vx, vy), 1, function(x, ...) fun(x[nc1], x[nc2], ...))
				out <- writeValues(out, v, tr$row[i])
			}
			return(writeStop(out))
		}
	}
)

