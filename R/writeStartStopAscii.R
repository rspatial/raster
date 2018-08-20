# Author: Robert J. Hijmans
# Date :  May 2010
# Version 0.9
# Licence GPL v3


.startAsciiWriting <- function(x, filename, NAflag, ...) {
 	filename <- trim(filename)
	if (filename == '') {
		stop('provide a filename')
	}
	x@file@name <- filename
	x@file@driver <- 'ascii'

	overwrite <- .overwrite(...)
	dtype  <- .shortDataType(.datatype(...))
	x@file@datanotation = .datatype(...)
	dtype  <- .shortDataType(x@file@datanotation)
	attr(x@file, "dtype") <- dtype


	if (!missing(NAflag)) { 
		x@file@nodatavalue <- NAflag
	} else if (!is.finite( x@file@nodatavalue) ) {
		x@file@nodatavalue <- -3.4e+38
	}

	
	resdif <- abs((yres(x) - xres(x)) / yres(x) )
	if (resdif > 0.01) {
		stop(paste("x has unequal horizontal and vertical resolutions. Such data cannot be stored in arc-ascii format"))
	} else if (resdif > 0.001) {
		warning("ignoring the slightly unequal horizontal and vertical resolutions")
	}
	if (!overwrite & file.exists(filename)) {
		stop(paste(filename, "exists. Use 'overwrite=TRUE'")) 
	}
	thefile <- file(filename, "w")  # open an txt file connection
	cat("NCOLS", ncol(x), "\n", file = thefile)
	cat("NROWS", nrow(x), "\n", file = thefile)
	cat("XLLCORNER", as.character(xmin(x)), "\n", file = thefile)
	cat("YLLCORNER", as.character(ymin(x)), "\n", file = thefile)
	cat("CELLSIZE",  as.character(xres(x)), "\n", file = thefile)
	cat("NODATA_value", x@file@nodatavalue, "\n", file = thefile)
	close(thefile) #close connection
	
	return(x)
	
}


.stopAsciiWriting <- function(x) {
	x@data@haveminmax <- TRUE
	if (x@file@dtype == "INT") {
		x@data@min <- round(x@data@min)
		x@data@max <- round(x@data@max)
#	} else if ( x@file@dtype =='LOG' ) { 
#		raster@data@min <- as.logical(raster@data@min)
#		raster@data@max <- as.logical(raster@data@max)
	}
	
	return( raster( x@file@name ) )

}		

 