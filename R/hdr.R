# Author: Robert J. Hijmans
# Date : October 2008
# Version 0.9
# Licence GPL v3

 
hdr <- function(x, format, extension='.wld', filename='') {

	if (inherits(x, 'RasterStack')) { stop('Only applicable to RasterLayer and RasterBrick classes (and their derivates)') }

	if (x@file@name == '') { 
		if (filename == '') {
			stop('Object has no filename; and none provided as argument') 
		} else {
			x@file@name = filename
		}
	}

#	if (missing(filename)) {
#		if (x@file@name == '') { 
#			stop('Object has no filename; please provide a "filename=" argument') 
#		}
#	} else {
#		fn <- trim(as.character(filename[1]))
#		if (nchar(fn) < 1) {
#			stop('invalid filename')
#		}
#		x@file@name == fn
#	}
	
	type <- toupper(format)
	if (type=="RASTER") {
		.writeHdrRaster(x)
	} else if (type=="WORLDFILE") {
		.worldFile(x, extension)		
	} else if (type=="VRT") {
		.writeHdrVRT(x)
		.writeStx(x)		
	} else if (type=="BIL") {
		.writeHdrBIL(x)
		.writeStx(x)
	} else if (type=="BSQ") {
		.writeHdrBIL(x, "BSQ")
		.writeStx(x)
	} else if (type=="BIP") {
		.writeHdrBIL(x, "BIP")
		.writeStx(x)
	} else if (type=="ERDASRAW") {
		.writeHdrErdasRaw(x)
		.writeStx(x)
	} else 	if (type=="ENVI") {
		.writeHdrENVI(x)
		.writeStx(x)
	} else 	if (type=="SAGA") {
		.writeHdrSAGA(x)
	} else 	if (type=="IDRISI") {
		.writeHdrIDRISI(x)
	} else 	if (type=="IDRISIold") {
		.writeHdrIDRISI(x, old=TRUE)
	} else 	if (type=="PRJ") {
		.writeHdrPRJ(x, ESRI=TRUE)
	} else {
		stop("This file format is not supported")
	}
	return( invisible(TRUE) )
 }

 
 
.writeStx <- function(x, filename='') {
	if (x@data@haveminmax) {
		if (filename=='') {
			filename <- filename(x)
		} 
		if (filename!='') {
			extension(filename) <- ".stx"
			thefile <- file(filename, "w")  # open a txt file connectionis
			cat(1, " ", minValue(x), " ", maxValue(x), "\n", file = thefile)
			close(thefile)
		}
	}	
	return( invisible(TRUE) )
}
 