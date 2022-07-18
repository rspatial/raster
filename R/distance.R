# Author: Robert J. Hijmans
# Date : September 2009
# Version 0.9
# Licence GPL v3


setMethod('distance', signature(x='RasterLayer', y='missing'), 
function(x, y, filename='', doEdge=TRUE, ...) {
	if (doEdge) {
		r <- boundaries(x, classes=FALSE, type='inner', progress=.progress(...)) 
		pts <- try(  rasterToPoints(r, fun=function(z){ z>0 } )[,1:2, drop=FALSE] )
	} else {
		pts <- try(  rasterToPoints(x)[,1:2, drop=FALSE] )
	}
	
	if (inherits(pts, "try-error")) {
		return( .distanceRows(x, filename=filename, ...) )
	}
	if (nrow(pts) == 0) {
		stop('RasterLayer has no NA cells (for which to compute a distance)')
	}
	out <- raster(x)
	filename <- trim(filename)
	
	if (couldBeLonLat(x)) { 
		longlat=TRUE 
	} else { 
		longlat=FALSE 
	}
	                                                                        
	if (canProcessInMemory(out, 6)) {
		pb <- pbCreate(3, label='distance', ...)
		x <- values(x)
		i <- which(is.na(x))
		if (length(i) < 1) {
			stop('raster has no NA values to compute distance to')
		}
		pbStep(pb)
		x[] <- 0
		xy <- xyFromCell(out, i)
		x[i] <- .Call('_raster_distanceToNearestPoint', xy, pts, longlat, 6378137.0, 1/298.257223563, PACKAGE='raster')
		pbStep(pb)
		out <- setValues(out, x)
		if (filename != '') {
			out <- writeRaster(out, filename=filename, ...)
		}
		pbStep(pb)
		pbClose(pb)
		return(out)
	} 
	
	out <- writeStart(out, filename=filename, ...)
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, label='distance', ...)
	xy <- cbind(rep(xFromCol(out, 1:ncol(out)), tr$nrows[1]), NA)
	for (i in 1:tr$n) {
		if (i == tr$n) {
			xy <- xy[1:(ncol(out)*tr$nrows[i]), ]
		}
		xy[,2] <- rep(yFromRow(out, tr$row[i]:(tr$row[i]+tr$nrows[i]-1)), each=ncol(out))
		vals <- getValues(x, tr$row[i], tr$nrows[i])
		j <- which(is.na(vals))
		vals[] <- 0
		if (length(j) > 0) {
			vals[j] <- .Call('_raster_distanceToNearestPoint', xy[j,,drop=FALSE], pts, longlat, 6378137.0, 1/298.257223563, PACKAGE='raster')	
		}
		out <- writeValues(out, vals, tr$row[i])
		pbStep(pb) 	
	}	
	pbClose(pb)
	out <- writeStop(out)
	return(out)
}
)


setMethod('distance', signature(x='RasterLayer', y='RasterLayer'), 
function(x, y, ...) {
	stats::dist(as.matrix(stack(x, y)))
}
)

setMethod('distance', signature(x='Spatial', y='Spatial'), 
function(x, y, ...) {

#	valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
#	stopifnot(inherits(x, 'SpatialVector'))
#	stopifnot(inherits(y, 'SpatialVector'))	
#	d <- rgeos::gDistance(x, y, byid=TRUE)
#	apply(d, 1, min)
	x = vect(x)
	y = vect(y)
	distance(x, y)
}
)

