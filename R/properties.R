# Author: Robert J. Hijmans
# Date :  October 2008
# Version 0.9
# Licence GPL v3


.driver <- function(object, warn=TRUE) {
	if (inherits(object, 'RasterStack')) {
		d <- sapply(object@layers, function(x) x@file@driver)
		if (any(d == '' & warn)) {
			warning('There is no driver associated with one or more layers of this RasterStack')
		}
	} else {
		d <- object@file@driver
		if (d == '' & warn) {
			warning('no file/driver associated with this Raster object')
		} 
	}
	return(d)
}



.nodatavalue <- function(object) {
	if (inherits(object, 'RasterStack')) {
		return( sapply(object@layers, function(x) x@file@nodatavalue) )
	}
	return(object@file@nodatavalue)
}	


filename <- function(x) {
	if (inherits(x, 'RasterStack')) { 
		return(x@filename) 
	} 
	return(x@file@name)
}



	
#	fileext <- toupper(extension(fn)) 
#	if ( fileext == ".GRD" | fileext == ".GRI" ) {
#		return('raster')
#	} else {
#		return('gdal')
#	}

#	fcon <- class(try( object@file@con, silent = T ))[1]
#	if (fcon == 'file') {
#		return('raster')
#	} else if (fcon == "GDALReadOnlyDataset") {
#		return('gdal')
#	} else if (fcon == "try-error") {
#		return('NA')
#	} else {
#		stop('unknown driver')
#	}

	

