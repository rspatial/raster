# Robert J. Hijmans
# May 2010
# Version 1.0
# Licence GPL v3


rotated <- function(x) {
	isTRUE(try(x@rotated, silent=TRUE))
}

setMethod("rectify", signature(x="Raster"), 
	function(x, ext, res, method='ngb', filename='', ...) {
		stopifnot(rotated(x))
		if ( missing(ext)) {
			ext <- extent(x)
		} else {
			ext <- extent(ext)
		}
		out <- raster(ext)
		if ( missing(res)) {
			res(out) <- abs(raster::res(x))
		} else {
			res(out) <- res
		}
		resample(x, out, method=method, filename=filename, ...)
	}
)


