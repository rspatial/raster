# Author: Robert J. Hijmans
# Date :  October 2009
# Version 0.9
# Licence GPL v3
 
.writeHdrIDRISI <- function(x, old=FALSE) {
	hdrfile <- filename(x)
	hdrfile <- .setFileExtensionHeader(hdrfile, 'IDRISI')

	dtype <- .shortDataType(x@file@datanotation)
	dsize <- dataSize(x)
	if (dataType(x) == 'INT1U') {
		pixtype <- 'byte'
	} else if (dataType(x) == 'INT2S') {
		pixtype <- 'integer'
	} else { 
		pixtype <- 'real'
	}

	if (couldBeLonLat(x)) {
		refsystem <- 'latlong'
		refunits <- 'degrees';
	} else {
		refsystem <- 'plane';
		refunits <- 'm';
	}
	
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	if (!old) cat('file format : IDRISI Raster A.1\n', file = thefile)
	cat('file title  : ', names(x), "\n", sep='', file = thefile)
	cat('data type   : ', pixtype, "\n", sep='', file = thefile)
	cat('file type   : binary\n', sep='', file = thefile)
	cat('columns     : ', ncol(x), "\n", sep='', file = thefile)
	cat('rows        : ', nrow(x), "\n", sep='', file = thefile)
	cat('ref. system : ', refsystem, "\n", sep='', file = thefile)
	cat('ref. units  : ', refunits, "\n", sep='', file = thefile)
	cat('unit dist.  : 1.0000000', "\n", sep='', file = thefile)
	cat('min. X      : ', as.character(xmin(x)), "\n", sep='', file = thefile)
	cat('max. X      : ', as.character(xmax(x)), "\n", sep='', file = thefile)
	cat('min. Y      : ', as.character(ymin(x)), "\n", sep='', file = thefile)
	cat('max. Y      : ', as.character(ymax(x)), "\n", sep='', file = thefile)
	cat("pos'n error : unknown\n", file = thefile)
	cat('resolution  : ', xres(x), "\n", sep='', file = thefile)
	cat('min. value  : ', minValue(x), "\n", sep='', file = thefile)
	cat('max. value  : ', maxValue(x), "\n", sep='', file = thefile)
  	if (!old) cat('display min : ', minValue(x), "\n", sep='', file = thefile)
  	if (!old) cat('display max : ', maxValue(x), "\n", sep='', file = thefile)
	cat('value units : unspecified\n', file = thefile)
	cat('value error : unknown\n', file = thefile)
	cat('flag value  : ', .nodatavalue(x), "\n", sep='', file = thefile)
	cat("flag def'n  : no data\n", file = thefile)
	cat('legend cats : 0\n', file = thefile)

	close(thefile)
	
	return(invisible(TRUE))
}
