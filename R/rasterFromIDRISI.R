# Author: Robert J. Hijmans
# Date : October 2009
# Version 0.9
# Licence GPL v3

.rasterFromIDRISIFile <- function(filename, crs="", old=FALSE, ...) {

	if (old) {
		idformat <- 'IDRISIold'
	} else {
		idformat <- 'IDRISI'
	}
	valuesfile <- .setFileExtensionValues(filename, idformat)
	if (!file.exists(valuesfile )){
		stop( paste(valuesfile,  "does not exist"))
	}		
	filename <- .setFileExtensionHeader(filename, idformat)
	
	ini <- readIniFile(filename, token=':')

	ini[,2] = toupper(ini[,2]) 

	byteorder <- .Platform$endian
	nodataval <- -Inf
	layernames <- ''
	filetype <- ''
	
	for (i in 1:length(ini[,1])) {
		if (ini[i,2] == "MIN. X") {xn <- as.numeric(ini[i,3])
		} else if (ini[i,2] == "MAX. X") {xx <- as.numeric(ini[i,3])
		} else if (ini[i,2] == "MIN. Y") {yn <- as.numeric(ini[i,3])
		} else if (ini[i,2] == "MAX. Y") {yx <- as.numeric(ini[i,3])
		} else if (ini[i,2] == "MIN. VALUE") { minval <-  as.numeric(ini[i,3]) 
		} else if (ini[i,2] == "MAX. VALUE") { maxval <-  as.numeric(ini[i,3]) 
		} else if (ini[i,2] == "VALUE UNITS") { valunit <-  ini[i,3] 
		} else if (ini[i,2] == "ROWS") {nr <- as.integer(ini[i,3])
		} else if (ini[i,2] == "COLUMNS") {nc <- as.integer(ini[i,3])
		} else if (ini[i,2] == "DATA TYPE") {inidatatype <- toupper(ini[i,3])
		} else if (ini[i,2] == "FILE TYPE") {filetype <- toupper(ini[i,3])
		} else if (ini[i,2] == "FILE TITLE") {layernames <- ini[i,3]
		} else if (ini[i,2] == "FLAG VALUE") { 
			suppressWarnings(nodataval <- try(as.numeric(ini[i,3], silent=TRUE)))
			if (!is.numeric(nodataval)) {nodataval <- -Inf}
		}
    }  
	
	if (filetype=='PACKED BINARY') {
		stop('cannot natively read packed binary files')
	}
	
	# attempt could be made to decipher some of the idrisi crs descriptions
	
	x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs=crs)

	if (nchar(layernames) > 1) {
		# lnams <- unlist(strsplit(layernames, ':'))
		lnams <- layernames
	} else {
		lnams <- gsub(" ", "_", extension(basename(filename), ""))
	}
	names(x) <- lnams
	
	x@file@name <- filename
	x@data@min <- minval
	x@data@max <- maxval
	x@data@haveminmax <- TRUE

	if (inidatatype == 'BYTE') {
		dataType(x) <- 'INT1U'
	} else if (inidatatype == 'INTEGER') {
		dataType(x) <- 'INT2S'
	} else if (inidatatype == 'REAL') {
		dataType(x) <- 'FLT4S'
	} else {
		stop(paste('unsupported IDRISI data type:', inidatatype))
	}
	
	x@file@nodatavalue <- nodataval
	x@data@fromdisk <- TRUE

	x@file@driver <- idformat
    return(x)
}



