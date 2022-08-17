# Author: Robert J. Hijmans
# Date : June 2008
# Version 0.9
# Licence GPL v3


	

setMethod('readStart', signature(x='Raster'), 
	function(x, ...) {
		if ( fromDisk(x) ) {
			return (.openConnection(x, ...))
		} else {
			return(x)
		}
	}
)


setMethod('readStart', signature(x='RasterStack'), 
	function(x, ..., maxopen=100) {
		fd <- sapply(x@layers, fromDisk)
		ld <- sum(fd)
		if (isTRUE( ld > 0 & ld <= maxopen)) {
			d <- which(fd)
			for (i in d) {
				x@layers[[i]] <- readStart(x@layers[[i]], con.check=103, ...)
			}
		}
		x
	}
)




.openConnection <- function(x, silent=TRUE, con.check=Inf, ...) {
	fn <- trim(x@file@name)
	driver <- .driver(x)
	if (driver == "gdal") {
#		attr(x@file, "con") <- rgdal::GDAL.open(fn, silent=silent)
#		x@file@open <- TRUE
	} else 	if (.isNativeDriver(driver))  {
		# R has a max of 128 connections
		if (length(getAllConnections()) < con.check) {
			fn <- .setFileExtensionValues(fn, driver)
			attr(x@file, "con") <- file(fn, "rb")
			x@file@open <- TRUE
		}
	} else if (driver == 'netcdf') {
		attr(x@file, 'con') <- ncdf4::nc_open(x@file@name, suppress_dimvals = TRUE)
		x@file@open <- TRUE
#	} else if (driver == 'ascii') { # cannot be opened
	}	
	x
}



setMethod('readStop', signature(x='Raster'), 
	function(x) {
		if ( fromDisk(x) ) {
			return (.closeConnection(x))
		} else {
			return(x)
		}
	}
)

setMethod('readStop', signature(x='RasterStack'), 
	function(x) {
		d <- which(sapply(x@layers, fromDisk))
		if (length(d) > 0) {
			for (i in d) {
				x@layers[[i]] <- readStop(x@layers[[i]])
			}
		}
		x
	}
)


.closeConnection <- function(x) {
	driver <- .driver(x)
	if (driver == "gdal") {
		#try( rgdal::closeDataset(x@file@con), silent = TRUE )
	} else if (.isNativeDriver(driver))  {
		try( close(x@file@con), silent = TRUE )
	} else if (driver == 'netcdf') {
		try( ncdf4::nc_close(x@file@con), silent=TRUE)
	} #else if (driver == 'ascii') {	}
	
	x@file@open <- FALSE
	attr(x@file, 'con') <- NULL
	x
#	attr(x@file, "con" <- "")
}

