# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3


.writeGDALall <- function(x, filename, filetype="GTiff", overwrite=FALSE, options=NULL, setStatistics=TRUE, ...) {

	stat <- cbind(NA, NA)
	if (nlayers(x) > 1) {
		y <- brick(x, values=FALSE)
		levels(y) <- levels(x)
		x <- getValues(x)
#		if (setStatistics) { 
#			stat <- t(apply(x, 2, function(z, ...) cbind(mean(z, na.rm=TRUE), stats::sd(z, na.rm=TRUE))))
#		}
	} else {
		y <- raster(x)
		levels(y) <- levels(x)
		y@legend@colortable <- colortable(x)
		x <- getValues(x)
#		if (setStatistics) { 
#			stat <- cbind(mean(x, na.rm=TRUE), stats::sd(x, na.rm=TRUE))
#		}
	}
	
	y <- .startGDALwriting(y, filename, gdal=options, filetype=filetype, overwrite=overwrite, ...)
	x <- writeValues(y, x, start=1)
	.stopGDALwriting(x, stat)
}
	
