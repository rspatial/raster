# Author: Robert J. Hijmans
# Date : September 2009
# revised October 2011
# Version 1.0
# Licence GPL v3

	

setMethod('direction', signature(x='RasterLayer'), 
function(x, filename='', degrees=FALSE, from=FALSE, doEdge=FALSE, ...) {

	out <- raster(x)
	if (couldBeLonLat(out)) { 
		longlat=TRUE 
	} else { 
		longlat=FALSE 
	}
	
	if (doEdge) {
		r <- boundaries(x, classes=FALSE, type='inner', asNA=TRUE, progress=.progress(...)) 
		pts <- try(  rasterToPoints(r, fun=function(z){ z>0 } )[,1:2, drop=FALSE] )
	} else {
		pts <- try(  rasterToPoints(x)[,1:2, drop=FALSE] )
	}
	if (inherits(pts, "try-error")) {
		stop('This function has not yet been implemented for very large files')
	}
	if (nrow(pts) == 0) {
		stop('RasterLayer has no NA cells (for which to compute a direction)')
	}
	
	filename <- trim(filename)
	if ( canProcessInMemory(out, 3)) {
		vals <- getValues(x)
		i <- which(is.na(vals))
		xy <- xyFromCell(out, i)
		vals[] <- NA
		vals[i] <- .Call('_raster_directionToNearestPoint', xy, pts, longlat, degrees, from, a=6378137.0, f=1/298.257223563, PACKAGE='raster')
		out <- setValues(out, vals)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		}
		return(out)
	}
	
	out <- writeStart(out, filename, ...)
	tr <- blockSize(out)
	pb <- pbCreate(tr$n, label='direction', ...)
	xy <- cbind(rep(xFromCol(out, 1:ncol(out)), tr$nrows[1]), NA)
	for (i in 1:tr$n) {
		if (i == tr$n) {
			xy <- xy[1:(ncol(out)*tr$nrows[i]), ]
		}
		xy[,2] <- rep(yFromRow(out, tr$row[i]:(tr$row[i]+tr$nrows[i]-1)), each=ncol(out))
		vals <- getValues(x, tr$row[i], tr$nrows[i])
		j <- which(is.na(vals))
		vals[] <- NA
		if (length(j) > 0) {
			vals[j] <- .Call('_raster_directionToNearestPoint', xy[j, ,drop=FALSE], pts, longlat, degrees, from, a=6378137.0, f=1/298.257223563, PACKAGE='raster')
		}
		out <- writeValues(out, vals, tr$row[i])
		pbStep(pb) 	
	}	
	pbClose(pb)
	out <- writeStop(out)
	return(out)
}
)


