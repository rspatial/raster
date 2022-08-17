# Author: Robert J. Hijmans
# Date : October 2009
# Version 0.9
# Licence GPL v3


.rasterFromGenericFile <- function(filename, band=1, SIGNEDINT=NULL, type='RasterLayer', crs="", ...) {
	hdrfname <- .setFileExtensionHeader(filename, "BIL")
	
	ini <- readIniFile(hdrfname, token=' ')
	
	if (ini[1,1] == "ENVI") {
		stop("This file has an ENVI header; I cannot read that natively, only via GDAL")
	}

	ini[,2] = toupper(ini[,2]) 

	byteorder <- ''
	nbands <- as.integer(1)
	band <- as.integer(band)
	bandorder <- "BIL"
	minval <- Inf
	maxval <- -Inf
	nodataval <- -Inf
	pixtype <- ''
	gaps <- 0
	xx <- xn <- xd <- yx <- yn <- yd <- NULL
	
	for (i in 1:length(ini[,1])) {
		if (ini[i,2] == "LLXMAP") {xn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "ULXMAP") {xn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "LRXMAP") {xx <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "URXMAP") {xx <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "LLYMAP") {yn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "ULYMAP") {yx <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "LRYMAP") {yn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "URYMAP") {yx <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "XDIM") {xd <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "YDIM") {yd <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "YMAX") {yx <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "ROWS") {nr <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "COLUMNS") {nc <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "NROWS") {nr <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "NCOLS") {nc <- as.integer(ini[i,3])} 
		
		else if (ini[i,2] == "NODATA") {nodataval <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "NBITS") {nbits <- ini[i,3]} 
		else if (ini[i,2] == "PIXELTYPE") {pixtype <- ini[i,3]} 
		else if (ini[i,2] == "BANDGAPBYTES") {gaps <- ini[i,3]} 
		
		else if (ini[i,2] == "BYTEORDER") {byteorder <- ini[i,3]} 
		else if (ini[i,2] == "NBANDS") {nbands <- ini[i,3]} 
		else if (ini[i,2] == "LAYOUT") {bandorder <- ini[i,3]} 
		else if (ini[i,2] == "MINVALUE=") {try (minval <- as.numeric(unlist(strsplit(trim(ini[i,3]), ' ')))) } 
		else if (ini[i,2] == "MAXVALUE=") {try (maxval <- as.numeric(unlist(strsplit(trim(ini[i,3]), ' ')))) } 
    }  


	wrldf <- extension(filename, '.blw')
	if (file.exists(wrldf)) {
		a <- readLines(wrldf)
		if (is.null(xn)) xn <- as.numeric(a[5])
		if (is.null(xd)) xd <- as.numeric(a[1])
		if (is.null(yx)) yx <- as.numeric(a[6])
		if (is.null(yd)) yd <- -1 * as.numeric(a[4])
	}

	
	if (is.null(xd)) {
		xd <- (xx - xn) / (nc - 1)
	}
	if (is.null(yd)) {
		yd <- (yx - yn) / (nr - 1)
	}

	if (!is.null(xn)) {
		xn <- xn - 0.5 * xd
		if (is.null(xx)) {
			xx <- xn + nc * xd
		} 
	} else {
		xx <- xx + 0.5 * xd
		xn <- xx - nc * xd
	} 
	
	if (!is.null(yn)) {
		yn <- yn - 0.5 * yd
		if (is.null(yx)) {
			yx <- yn + nr * yd
		} 
	} else {
		yx <- yx + 0.5 * yd
		yn <- yx - nr * yd
	}
	
	if (gaps > 0) { stop('generic raster with gaps not supported') }
	
	if (band < 1) {
		band <- 1
		warning('band set to 1')
	} else if  (band > nbands) {
		band <- nbands
		warning('band set to ', nbands)
	}
	
	minval <- minval[1:nbands]
	maxval <- maxval[1:nbands]
	minval[is.na(minval)] <- Inf
	maxval[is.na(maxval)] <- -Inf

	if (type == 'RasterBrick') {
		x <- brick(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs=crs)
		x@data@nlayers <-  as.integer(nbands)
		x@data@min <- minval
		x@data@max <- maxval
	} else {
		x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs=crs)
		x@data@band <- as.integer(band)
		x@data@min <- minval[band]
		x@data@max <- maxval[band]
	}
	if (x@data@min[1] != Inf) {x@data@haveminmax <- TRUE
	} else 	{ x@data@haveminmax <- FALSE }


	x@file@nbands <- as.integer(nbands)

	if (bandorder %in% c("BSQ", "BIP", "BIL")) {
		x@file@bandorder <- bandorder 
	}

	if (type == 'RasterBrick') {
		names(x) <- rep(gsub(" ", "_", extension(basename(filename), "")), nbands)
	} else {
		lnames <- gsub(" ", "_", extension(basename(filename), ""))
		if (nbands > 1) {
			lnames <- paste(lnames, '_', band, sep='')
		}
		names(x) <- lnames
	}

	x@file@name <- filename

	if (!is.null(SIGNEDINT)) {
		if(SIGNEDINT) { pixtype <- 'SIGNEDINT' 
		} else { pixtype <- 'UNSIGNEDINT' 
		}
	}
	if (nbits == 8) {
		if (pixtype == 'SIGNEDINT') {
			dataType(x) <- 'INT1S'
		} else {
			if (pixtype != 'UNSIGNEDINT') {
				warning('assuming data is unsigned. If this is not correct, use  dataType(x) <- "INT1S"')
			}
			dataType(x) <- 'INT1U'		
		}
	} else if (nbits == 16) {
		if (pixtype == 'SIGNEDINT') {
			dataType(x) <- 'INT2S'
		} else {
			if (pixtype != 'UNSIGNEDINT') {
				warning('assumed data is unsigned. If this is not correct, use  dataType(x) <- "INT2S"')
			}
			dataType(x) <- 'INT2U'		
		}
	} else if (nbits == 32) {
		if (pixtype == 'FLOAT') {
			dataType(x) <- 'FLT4S'
		} else {
			dataType(x) <- 'INT4S'
		}
	} else if (nbits == 64 & pixtype == 'FLOAT') {
		dataType(x) <- 'FLT8S'
#		} else {
#			dataType(x) <- 'INT8S'
#		}
	} else {
		stop(paste('unexpected nbits in BIL:', nbits))
	}
	
	if (byteorder == "I") { 
		x@file@byteorder <- 'little'
	} else if (byteorder == "M") { 
		x@file@byteorder <- 'big'
	} else {
		x@file@byteorder <- .Platform$endian
	}
	
	x@data@fromdisk <- TRUE
	
	x@file@driver <- bandorder
	x@file@nodatavalue <- nodataval

    return(x)
}

