# Author: Robert J. Hijmans
# Date : September 2009
# Version 0.9
# Licence GPL v3

distanceFromPoints <- function(object, xy, filename='', ...) {
	
	pts <- .pointsToMatrix(xy)

	filename <- trim(filename)
	
	if (couldBeLonLat(object)) { 
		longlat=TRUE 
	} else { 
		longlat=FALSE 
	}
	                                                                        
	out <- raster(object)
	if (canProcessInMemory(out, 4)) {
		xy <- xyFromCell(out, 1:ncell(out))
		a = 6378137.0
		f = 1/298.257223563
		out <- setValues(out, .Call('_raster_distanceToNearestPoint', xy,  pts, longlat, a, f , PACKAGE = 'raster'))

		if (filename != '') {
			out <- writeRaster(out, filename=filename, ...)
		}
		return(out)
	} 
	
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, ...)
	xy <- cbind(rep(xFromCol(out, 1:ncol(out)), tr$nrows[1]), NA)
	for (i in 1:tr$n) {
		if (i == tr$n) {
			xy <- xy[1:(ncol(out)*tr$nrows[i]), ]
		}
		xy[,2] <- rep(yFromRow(out, tr$row[i]:(tr$row[i]+tr$nrows[i]-1)), each=ncol(out))
		vals <- .Call('_raster_distanceToNearestPoint', xy, pts, longlat, 0.0, 0.0, PACKAGE='raster')
		out <- writeValues(out, vals, tr$row[i])
		pbStep(pb) 	
	}	
	pbClose(pb)
	out <- writeStop(out)
	return(out)
}

