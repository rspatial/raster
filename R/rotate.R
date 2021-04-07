# Author: Robert J. Hijmans
# Date : September 2009
# Version 0.9
# Licence GPL v3



setMethod('rotate', signature(x='Raster'), 
	function(x, filename='', ...) {
		
		e <- extent(x)
		
		if (e@xmin < -60) {
			warning('xmin is much smaller than zero. No rotation done')
			return(x)
		}

		xrange <- e@xmax - e@xmin
		if (xrange < 350 | xrange > 370 | e@xmin < -10 | e@xmax > 370) {
			if (xrange < 350 | xrange > 370 | e@xmin < -190 | e@xmax > 190) {	 
				warning('this does not look like an appropriate object for this function')
			}
		}
		
		xr <- xres(x)
		ext1 <- extent(-xr, 180, -100, 100)
		if (is.null(intersect(e, ext1 ))) {
			r1 <- NULL
		} else {
			r1 <- crop(x, ext1)
		}		
		ext2 <- extent(180, 360+xr, -100, 100)
		if (is.null(intersect(e, ext2 ))) {
			r2 <- NULL
		} else {
			r2 <- crop(x, ext2)
			r2 <- shift(r2, -360)
		}
		ln <- names(x)
		if (is.null(r1)) {
			out <- r2
		} else if (is.null(r2)) {
			out <- r1		
		} else {
			out <- merge(r1, r2, overlap=FALSE)
		}
		names(out) <- names(x)
		out@z <- x@z
		
		# suggested by Mike Sumner:
		p <- proj4string(out)	
		if (length(grep("\\+over", p)) > 0) {
			projection(out) <- gsub("[[:space:]]\\+over", "", p)
		}
		
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		}
		return(out)
	}
)

