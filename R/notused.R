# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


# Not used
.writeRasterAssign <- function(x, filename, ...) {
	name <- deparse(substitute(x))
	x <- writeRaster(x, filename, ...)
	assign(name, x, envir=parent.frame())
	return(invisible())
}


.writeSparse <- function(raster, filename, overwrite=FALSE) {

#	raster@file@driver <- 'raster'
	if (!overwrite & file.exists(filename)) {
		stop(filename, "exists. Use 'overwrite=TRUE' if you want to overwrite it") 
	}

	raster@data@values[is.nan(raster@data@values)] <- NA

	dtype <- .shortDataType(raster@data@datanotation)
	if (dtype == "integer") { 
		raster@data@values <- as.integer(raster@data@values) 
	}
	if (class(raster@data@values)=='integer') {
		dataType(raster) <- 'INT4S'
	}	
	raster <- setMinMax(raster)

	binraster <- .setFileExtensionValues(raster@file@name, 'raster')

	raster <- readStart(raster)
	writeBin( as.vector(raster@data@indices), raster@file@con, size = as.integer(4)) 
	writeBin( as.vector(raster@data@values), raster@file@con, size = dataSize(raster@file@datanotation) ) 
	raster <- readStop(raster)

	# add the 'sparse' key word to the hdr file!!!
	hdr(raster) 
	return(raster)
} 

