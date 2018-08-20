# Author: Robert J. Hijmans
# Date : June 2008
# Version 0.9
# Licence GPL v3



.addFiles <- function(x, rasterfiles, bands=rep(1, length(rasterfiles))) {
	if (length(bands) == 1) {
		bands=rep(bands, length(rasterfiles))
	} 
	rasters <- list()
	for (i in 1:length(rasterfiles)) { 
		if (bands[[i]] < 1) {
			r <- raster(rasterfiles[[i]], band=1)
			rasters <- c(rasters, r)
			if (nbands(r) > 1) {
				for (j in 2:nbands(r)) {
					r <- raster(rasterfiles[[i]], band=j)
					rasters <- c(rasters, r)
				}
			}
		} else {
			rasters <- c(rasters, raster(rasterfiles[[i]], FALSE, band=bands[[i]]))
		}
	}	
	x <- addLayer(x, rasters) 
	return(x)
}


