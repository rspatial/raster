# Author: Robert J. Hijmans
# Date : October 2008
# Version 0.9
# Licence GPL v3


.worldFile <- function(raster, extension=".wld") {
	hdrfile <- filename(raster)
	extension(hdrfile) <- extension
	thefile <- file(hdrfile, "w")  
	cat(as.character(xres(raster)), "\n", file = thefile)
	cat("0\n", file = thefile)
	cat("0\n", file = thefile)
	cat(-1 * yres(raster), "\n", file = thefile)
    cat(xmin(raster) + 0.5 * xres(raster), "\n", file = thefile) 
    cat(ymax(raster) - 0.5 * yres(raster), "\n", file = thefile) 
	close(thefile)	
}
