# Author: Robert J. Hijmans
# Date :  September 2009
# Version 0.9
# Licence GPL v3


.isSurferFile <- function(filename, version=FALSE) {
	con <- file(filename, "rb")
	id <- readBin(con, "character", n=1, size=4)
	close(con)
	if (id == 'DSBB') { 
		if (version) { 
			return(6) 
		} else { 
			return (TRUE) 
		}
	} 
	con <- file(filename, "rb")
	id <- readBin(con, "numeric", n=1, size=4)
	close(con)
	if (id == as.numeric(0x42525344)) {
		if (version) {
			return(7)
		} else {
			return (TRUE)
		}
	} else {
		return (FALSE) 
	}
}


.rasterFromSurferFile <- function(filename) {
	v <- .isSurferFile(filename, TRUE) 
	if (v == 6) {
		return ( .rasterFromSurfer6(filename) ) 
	} else if (v == 7) {
		return ( .rasterFromSurfer7(filename) ) 
	} else {
		stop ('not a (recognized) binary Surfer file')
	}
}


.rasterFromSurfer6 <- function(filename) {
	con <- file(filename, "rb")
	r <- raster()
	id <- readBin(con, "character", n=1, size=4)
	r@ncols <- readBin(con, "int", n=1, size=2)
	r@rows <- readBin(con, "int", n=1, size=2)
	r@extent@xmin <- readBin(con, "double", n=1, size=8)
	r@extent@xmax <- readBin(con, "double", n=1, size=8)
	r@extent@ymin <- readBin(con, "double", n=1, size=8)
	r@extent@ymax <- readBin(con, "double", n=1, size=8)
	r@data@min <-  readBin(con, "double", n=1, size=8)
	r@data@max <-  readBin(con, "double", n=1, size=8)
	close(con)
	r@file@offset <- 56
	r@file@toptobottom <- FALSE
	dataType(r) <- 'FLT4S'
	r@data@fromdisk <- TRUE
	
	r@file@driver <- "surfer"
	return(r)
}


.rasterFromSurfer7 <- function(filename) {
#  source: http://www.geospatialdesigns.com/surfer7_format.htm
	con <- file(filename, "rb")
	r <- raster()
	id <- readBin(con, "numeric", n=1, size=4)
	size <- readBin(con, "numeric", n=1, size=4)
	offset <- size + 8
	seek(con, size, origin = "current")	
	id <- readBin(con, "numeric", n=1, size=4)  
	if (id != as.numeric(0x44495247)) {
		# should be 0x44495247  grid section
		# get size and skip to the next section 
		stop('file with this section not yet supported')
	}
	size <- readBin(con, "numeric", n=1, size=4)
	offset <- offset + size + 8
	r@rows <- as.integer(readBin(con, "numeric", n=1, size=4))
	r@cols <- as.integer(readBin(con, "numeric", n=1, size=4))
	r@extent@xmin <- readBin(con, "double", n=1, size=8)
	r@extent@ymin <- readBin(con, "double", n=1, size=8)
	xr <- readBin(con, "double", n=1, size=8)
	yr <- readBin(con, "double", n=1, size=8)
	r@extent@xmax <- r@extent@xmin + xr * r@cols
	r@extent@ymax <- r@extent@ymin + yr * r@rows
	r@data@min <-  readBin(con, "double", n=1, size=8)
	r@data@max <-  readBin(con, "double", n=1, size=8)
	rotation <- readBin(con, "double", n=1, size=8)
	if (rotation != 0) {
		stop('rotation != 0, cannot use this file')
	}
	r@data@max <-  readBin(con, "double", n=1, size=8)
	r@file@nodatavalue <- readBin(con, "double", n=1, size=8)
	id <- readBin(con, "numeric", n=1, size=4)
	size <- readBin(con, "numeric", n=1, size=4)
	close(con)
	r@file@offset <- offset + 8
	r@file@toptobottom <- FALSE
	if (ncell(r) / size == 4) {
		dataType(r) <- 'FLT4S'
	} else 	if (ncell(r) / size == 8) {
		dataType(r) <- 'FLT8S'
	} else {
		stop('sorry; cannot process this file')
	}
	r@file@driver <- "surfer"
	return(r)
}


