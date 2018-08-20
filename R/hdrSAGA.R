# Author: Robert J. Hijmans
# Date :  October 2009
# Version 0.9
# Licence GPL v3
 
.writeHdrSAGA <- function(x) {
	hdrfile <- filename(x)
	hdrfile <- .setFileExtensionHeader(hdrfile, 'SAGA')
	
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("NAME\t=",  names(x), "\n", file = thefile)
	cat("DESCRIPTION\t= \n", file = thefile)
	cat("UNIT\t= \n", file = thefile)
	
	dtype <- .shortDataType(x@file@datanotation)
	dsize <- dataSize(x@file@datanotation)
	if (dtype == 'INT' ) { 
		if (dsize == 1) {
			pixtype <- "BYTE"
		} else if (dsize == 2) {
			pixtype <- "SHORTINT"
		} else if (dsize == 4) {
			pixtype <- "INTEGER"
		}
		if (! dataSigned(x@file@datanotation)) {
			pixtype <- paste(pixtype, "_UNSIGNED", sep="")
		}
	} else if ( x@file@datanotation == 'FLT4S' ) {
		pixtype <- "FLOAT" 		
	} else {
		stop(paste('cannot write SAGA file with data type:', x@file@datanotation))
	}
	
	cat("DATAFORMAT\t=", pixtype, "\n", file = thefile)
	
	cat("DATAFILE_OFFSET\t= 0\n", file = thefile)
	cat("BYTEORDER_BIG\t=", x@file@byteorder != 'little', "\n", file = thefile)

	cat("POSITION_XMIN\t= ",  as.character(xmin(x) + 0.5 * xres(x)), "\n", file = thefile)
	cat("POSITION_YMIN\t= ",  as.character(ymin(x) + 0.5 * yres(x)), "\n", file = thefile)

	cat("CELLCOUNT_Y\t= ",  nrow(x), "\n", file = thefile)
	cat("CELLCOUNT_X\t= ",  ncol(x), "\n", file = thefile)
	cat("CELLSIZE\t= ",  xres(x), "\n", file = thefile)
	cat("Z_FACTOR\t= 1.000000\n", file = thefile)
    cat("NODATA_VALUE\t=", .nodatavalue(x), "\n", file = thefile)	
    cat("TOPTOBOTTOM\t= TRUE", "\n", file = thefile)	
	close(thefile)
	
	return(invisible(TRUE))
}
