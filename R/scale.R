# Author: Robert J. Hijmans
# Date : September 2012
# Version 1.0
# Licence GPL v3


setMethod('scale', signature('Raster'), 

	function(x, center=TRUE, scale=TRUE) {
		
		if (canProcessInMemory(x)) {
			v <- values(x)
			x <- setValues(x, scale(v, center=center, scale=scale))
			return(x)		
		}
		
		if (!is.logical(center)) {
			
			stopifnot(length(center) == nlayers(x))
			x <- x - center
			
		} else if (center) {
			m <- cellStats(x, 'mean', na.rm=TRUE)
			x <- x - m
		}
		
		if (!is.logical(scale)) {
			stopifnot(length(scale) == nlayers(x))
			x <- x / scale
			
		} else if (scale) {
			if (center[1] & is.logical(center[1])) {
				st <- cellStats(x, 'sd', na.rm=TRUE)
			} else {
				st <- cellStats(x, 'rms', na.rm=TRUE)
			}
			x <- x / st
		}
		x
	}
)



