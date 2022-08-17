# Author: Robert J. Hijmans
# Date : September 2009
# Version 0.9
# Licence GPL v3


.distanceRows <- function(object, filename, progress='', ...) {

	filename <- trim(filename)
	overwrite <- .overwrite(...)

	if( (!overwrite) & file.exists(filename)) {
		stop('file exists; use overwrite=TRUE to overwrite it')
	}
	if (couldBeLonLat(object)) { longlat=TRUE } else { longlat=FALSE }

	e <- boundaries(object, classes=FALSE, type='inner', asNA=TRUE) 
	
	r <- raster(object)
	tr <- blockSize(r, n=3)
	tmp = rasterTmpFile()
	extension(tmp) = '.tif'
	
	#.requireRgdal()
	r <- writeStart(r, filename=tmp, format='GTiff')
	
	pb <- pbCreate(tr$n, progress=progress)			
	xx <- xFromCol( r, 1:ncol(r) )
	
	hasWritten=FALSE
	for (i in 1:tr$n) {
	# get the from points for a block
		v <- getValuesBlock(e, row=tr$row[i], nrows=tr$nrows[i])
		x <- rep(xx, tr$nrows[i])
		y <- yFromRow(r, tr$row[i]) - (0:(tr$nrows[i]-1)) * yres(r)
		y <- rep(y, each=ncol(r))
		xyv <- cbind(x,y,v)
		from <- stats::na.omit(xyv)[,1:2]
		if (isTRUE(nrow(from)==0)) {
			pbStep(pb, i) 			
			next
		}
		for (j in 1:tr$n) {
			# distance to these points for all blocks
			x <- rep(xx, tr$nrows[j])
			y <- yFromRow(r, tr$row[j]) - (0:(tr$nrows[j]-1)) * yres(r)
			y <- rep(y, each=ncol(r))
			v <- getValuesBlock(object, row=tr$row[j], nrows=tr$nrows[j])
			xyv <- cbind(x,y,v)
			to <- xyv[is.na(xyv[,3]), 1:2]
			v[] = 0
			if ( isTRUE(nrow(to) > 0) ) {
				v[is.na(xyv[,3])] <- .Call('_raster_distanceToNearestPoint', to, from, longlat, 6378137.0, 1/298.257223563, PACKAGE='raster')
			}			
			if (hasWritten) {
				# after the first round, compare new values with previously written values
				v <- pmin(v, .getTransientRows(r, tr$row[j], n=tr$nrows[j])) 
			} 
			r <- writeValues(r, v, tr$row[j])			
		}
		hasWritten = TRUE
		pbStep(pb, i) 			
	}
	r <- writeStop(r)
	pbClose(pb)
	
	r <- writeRaster(r, filename=filename, ...)
	return(r)
}	

