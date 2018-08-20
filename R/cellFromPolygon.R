# Author: Robert J. Hijmans
# Date : January 2011
# Version 1.0
# Licence GPL v3


cellFromPolygon <- function(object, p, weights=FALSE) {

	spbb <- bbox(p)
	rsbb <- bbox(object)
	addres <- max(res(object))
	npol <- length(p@polygons)
	res <- list()
	res[[npol+1]] = NA

	if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
		return(res[1:npol])
	}
	rr <- raster(object)
	for (i in 1:npol) {
		pp <- p[i,]
		spbb <- bbox(pp)
		
		if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
			# do nothing; res[[i]] <- NULL
		} else {
			rc <- crop(rr, extent(pp)+addres)
			if (weights) {
				rc <- .polygonsToRaster(pp, rc, getCover=TRUE, silent=TRUE)
				rc[rc==0] <- NA
				xy <- rasterToPoints(rc)
				weight <- xy[,3] / 100
				xy <- xy[,-3]
			} else {
				rc <- .polygonsToRaster(pp, rc, silent=TRUE)
				xy <- rasterToPoints(rc)[,-3,drop=FALSE]
			}
			
			if (length(xy) > 0)  {  # catch holes or very small polygons
				cell <- cellFromXY(object, xy)
				if (weights) {
					res[[i]] <- cbind(cell, weight)
				} else {
					res[[i]] <- cell
				}
			} 
		}
	}
	
	return( res[1:npol] )
}



