# R raster package
# Date : September 2009
# Version 1.0
# Licence GPL v3


.rasterObjectFromFile <- function(x, band=1, objecttype='RasterLayer', native=FALSE, silent=TRUE, offset=NULL, ncdf=FALSE, ...) {
	x <- trim(x)
	if (x=="" | x==".") { # etc?
		stop('provide a valid filename')
	}

	# fix for opendap https://r-forge.r-project.org/forum/message.php?msg_id=5015
	start <- tolower(substr(x, 1, 3))
	if (! start %in% c('htt', 'ftp')) {
		y <- NULL
		try( y <- normalizePath( x, mustWork=TRUE), silent=TRUE )
		if (! is.null(y)) {
			x <- y
		}
	}

	fileext <- toupper(extension(x))

	if ((fileext == ".GRD") || (fileext == ".GRI"))  {
		grifile <- .setFileExtensionValues(x, 'raster')
		grdfile <- .setFileExtensionHeader(x, 'raster')
		if ( file.exists( grdfile) && file.exists( grifile)) {
			return ( .rasterFromRasterFile(grdfile, band=band, objecttype, ...) )
		}
	}

	if (! file.exists(x) ) {
		if (extension(x) == '') {
			grifile <- .setFileExtensionValues(x, 'raster')
			grdfile <- .setFileExtensionHeader(x, 'raster')
			if ( file.exists( grdfile) & file.exists( grifile)) {
				return ( .rasterFromRasterFile(grdfile, band=band, objecttype, ...) )
			} else {
				# stop('file: ', x, ' does not exist')
			}
		}
	}

	#if (isTRUE(GMT)) {
	#	return(.rasterObjectFromCDF_GMT(x))
	#}
	if (( fileext %in% c(".HE5", ".NC", ".NCF", ".NC4", ".CDF", ".NCDF", ".NETCDF")) | (isTRUE(ncdf))) {
		return ( .rasterObjectFromCDF(x, type=objecttype, band=band, ...) )
	} 
	if ( fileext == ".GRD") {
		if (.isNetCDF(x)) {
			return ( .rasterObjectFromCDF(x, type=objecttype, band=band, ...) )
		} 
	}

#	if ( fileext == ".BIG" | fileext == ".BRD") {
#		return( .rasterFromRasterFile(x, band=band, objecttype, driver='big.matrix', ...) )
#	}

	if (!is.null(offset)) {
		return ( .rasterFromASCIIFile(x, offset, ...) )
	}

           ## MDSumner, NSIDC data
    if (fileext %in% c(".BIN")) {
        r <- .rasterFromNSIDCFile(x)
        if (!is.null(r)) return(r)
    }

#	if(!native) {
#		if (! .requireRgdal(FALSE) )  {
#			native <- TRUE
#		}
#	}
	if (native) {
		if ( fileext == ".ASC" ) {
			return ( .rasterFromASCIIFile(x, ...) )
		}
		if ( fileext %in% c(".BIL", ".BIP", ".BSQ")) {
			return ( .rasterFromGenericFile(x, type=objecttype, ...) )
		}
		if ( fileext %in% c(".RST", ".RDC") ) {
#  not tested much
			return ( .rasterFromIDRISIFile(x, ...) )
		}
		if ( fileext %in% c(".DOC", ".IMG") ) {
#  not tested much
			return ( .rasterFromIDRISIFile(x, old=TRUE, ...))
		}
		if ( fileext %in% c(".SGRD", ".SDAT") ) {
# barely tested
			return ( .rasterFromSAGAFile(x, ...) )
		}

	}

	# old IDRISI format
	if ( fileext == ".DOC" ) {
		if (file.exists( extension(x, '.img'))) {
			return( .rasterFromIDRISIFile(x, old=TRUE, ...))
		}
	}

	if ( fileext %in% c(".SGRD", ".SDAT") ) {
		r <-  .rasterFromSAGAFile(x, ...)
		if (r@file@toptobottom | r@data@gain != 1) {
			return(r)
		} # else use gdal
	}



	#if (! .requireRgdal(FALSE) ) {
	#	stop("Cannot create RasterLayer object from this file; perhaps you need to install rgdal first")
	#}
	test <- try( r <- .rasterFromGDAL(x, band=band, objecttype, ...), silent=silent )
	if (inherits(test, "try-error")) {
		if (!file.exists(x)) {
			stop("Cannot create a RasterLayer object from this file. (file does not exist)")
		}
		stop("Cannot create a RasterLayer object from this file.")
	} else {
		return(r)
	}
}

