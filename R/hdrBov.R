# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.9
# Licence GPL v3


.writeHdrBOV <- function(raster) {
	hdrfile <- filename(raster)
	extension(hdrfile) <- '.bov'
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("TIME: 1.23456", "\n", file = thefile)
	datf <- filename(raster)
	extension(datf) <- '.gri'
	cat("DATA_FILE:", datf, "\n", file = thefile)
	cat("DATA_SIZE:", nrow(raster), ncol(raster), nlayers(raster), "\n", file = thefile)
	
	dtype <- substr(raster@file@datanotation, 1, 3)
	if (dtype == 'INT' | dtype == 'LOG' ) { 
		pixtype <- "INT"
	} else { 
		pixtype <- "FLOAT" 
	}
	cat("DATA_FORMAT:", pixtype, "\n", file = thefile)
	cat("VARIABLE: ", basename(filename(raster)),  "\n", file = thefile)
	cat("BYTEORDER ", toupper(.Platform$endian), "\n", file = thefile)
	cat("CENTERING: zonal", "\n", file = thefile)
	cat("BRICK_ORIGIN:", xmin(raster), ymin(raster), "0.", "\n", file = thefile)
	cat("BRICK_SIZE:", xres(raster), yres(raster), "1.", "\n", file = thefile)

	close(thefile)
	return(invisible(TRUE))	
}
