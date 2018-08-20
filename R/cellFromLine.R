# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.9
# Licence GPL v3

cellFromLine <- function(object, lns) {
	spbb <- bbox(lns)
	rsbb <- bbox(object)
	addres <- 2 * max(res(object))
	nlns <- length( lns@lines )
	res <- list()
	res[[nlns+1]] = NA

	if (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) {
		return(res[1:nlns])
	}
	
	rr <- raster(object)
	for (i in 1:nlns) {
		pp <- lns[i,]
		spbb <- bbox(pp)
		
		if (! (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) ) {
			rc <- crop(rr, extent(pp)+addres)
			rc <- .linesToRaster(pp, rc, silent=TRUE)
			xy <- rasterToPoints(rc)[,-3,drop=FALSE]
			if (length(xy) > 0) { # always TRUE?
				res[[i]] <- cellFromXY(object, xy)
			} 
		}
	}

	return( res[1:nlns] )
}

