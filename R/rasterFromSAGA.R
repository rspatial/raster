# Author: Robert J. Hijmans
# Date : October 2009
# Version 0.9
# Licence GPL v3


.rasterFromSAGAFile <- function(filename, crs="", ...) {
	valuesfile <- .setFileExtensionValues(filename, "SAGA")
	if (!file.exists(valuesfile )){
		stop( paste(valuesfile,  "does not exist"))
	}	
	filename <- .setFileExtensionHeader(filename, "SAGA")
	
	ini <- readIniFile(filename)

	ini[,2] = toupper(ini[,2]) 

	byteorder <- .Platform$endian
	ncellvals <- -9
	nodataval <- -Inf
	layernames <- ''
	toptobottom <- FALSE
	dfoffset <- as.integer(0)
	zfactor <- 1
	
	for (i in 1:length(ini[,1])) {
		if (ini[i,2] == "POSITION_XMIN") { xn <- as.numeric(ini[i,3]) } 
		else if (ini[i,2] == "POSITION_YMIN") { yn <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "CELLCOUNT_Y") { nr <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "CELLCOUNT_X") { nc <- as.integer(ini[i,3])} 
		else if (ini[i,2] == "CELLSIZE") { cellsize <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "NODATA_VALUE") { nodataval <- as.numeric(ini[i,3])} 
		else if (ini[i,2] == "DATAFORMAT") { inidatatype <- ini[i,3]} 
		else if (ini[i,2] == "BYTEORDER_BIG") { byteorder <- as.logical(ini[i,3])} 
#		else if (ini[i,2] == "NCELLVALS") {ncellvals <- ini[i,3]} 
		else if (ini[i,2] == "NAME") { layernames <- ini[i,3]} 
		else if (ini[i,2] == "Z_FACTOR") { zfactor <-  as.numeric(ini[i,3])}
		else if (ini[i,2] == "TOPTOBOTTOM") { toptobottom <-  as.logical(ini[i,3])}
		else if (ini[i,2] == "DATAFILE_OFFSET") { dfoffset <-  as.integer(ini[i,3])}
    }  
	

	xx <- xn + nc * cellsize - (0.5 * cellsize)
	xn <- xn - (0.5 * cellsize)
	yx <- yn + nr * cellsize - (0.5 * cellsize)
	yn <- yn - (0.5 * cellsize)
	

	
	x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs=crs)

	x@file@offset <- dfoffset
	x@file@toptobottom <- toptobottom

	if (nchar(layernames) > 1) {
		lnams <- unlist(strsplit(layernames, ':'))
	} else {
		lnams <- gsub(" ", "_", extension(basename(filename), ""))
	}
	names(x) <- lnams
	
	x@file@name <- filename
	x@data@haveminmax <- FALSE
	x@file@nodatavalue <- nodataval

	if (inidatatype == 'BIT') {
		stop('cannot process BIT data')
	} else if (inidatatype == 'BYTE') {
		dataType(x) <- 'INT1S'
	} else if (inidatatype == 'BYTE_UNSIGNED') {
		dataType(x) <- 'INT1U'
	} else if (inidatatype == 'SHORTINT') {
		dataType(x) <- 'INT2S'
	} else if (inidatatype == 'SHORTINT_UNSIGNED') {
		dataType(x) <- 'INT2U'
	} else if (inidatatype == 'INTEGER') {
		dataType(x) <- 'INT4S'
	} else if (inidatatype == 'INTEGER_UNSIGNED') {
		dataType(x) <- 'INT4U'
	} else if (inidatatype == 'FLOAT') {
		dataType(x) <- 'FLT4S'
	} else if (inidatatype == 'DOUBLE') {
		dataType(x) <- 'FLT8S'
	}
	
	if (byteorder) { 
		x@file@byteorder <- 'big'
	} else  {
		x@file@byteorder <- 'little'
	}
	x@data@fromdisk <- TRUE
	x@data@gain <- zfactor
	
	x@file@driver <- 'SAGA'
    return(x)
}


