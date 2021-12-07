# return or change file extensions
# Author: Robert J. Hijmans
# Date : October 2008
# Version 1.0
# Licence GPL v3

extension <- function(filename, value=NULL, maxchar=10) {
	if (!is.null(value)) {
		extension(filename) <- value
		return(filename)
	}   
	lfn <- nchar(filename)
	ext <- list()
	for (f in 1:length(filename)) {
		extstart <- -1
		for (i in lfn[f] : 2) {
			if (substr(filename[f], i, i) == ".") {
				extstart <- i
				break
			}
		}
		if (extstart > 0) {
			ext[f] <- substr(filename[f], extstart, lfn[f])
		} else { 
			ext[f] <- "" 
		}   
	}
	ext <- unlist(ext)
	ext[nchar(ext) > maxchar] <- ''
	return(ext)
}   


'extension<-' <- function(filename, value) {
	value <- trim(value)
	if (value != "" & substr(value, 1, 1) != ".") {
		value <- paste(".", value, sep="") 
	}
	lfn <- nchar(filename)
	fname <- list()
	for (f in 1:length(filename)) {
		extstart <- -1
		for (i in lfn[f] : 2) {
			if (substr(filename[f], i, i) == ".") {
				extstart <- i
				break 
			}
		}
		if (extstart > 0 & (lfn[f] - extstart) < 8) {
			fname[f] <- paste(substr(filename[f], 1, extstart-1), value, sep="")
		} else { 
			fname[f] <- paste(filename[f], value, sep="")  
		}
	}
	return( unlist(fname) ) 
}   


.getExtension <- function(f, format) {
	if (.setfileext()) {
		def <- .defaultExtension(format, f)
		if (def != '') {
			extension(f) <- def
		}
	}
	return(f)
}



.defaultExtension <- function(format=.filetype(), filename="") {
	format <- toupper(format)
	if (format == 'RASTER') { return('.grd') 
	} else if (format == 'GTIFF') { 
		e <- extension(filename)
		if (tolower(e) %in% c(".tiff", ".tif")) {
			return (e)
		} else {
			return('.tif')
		}
	} else if (format == 'CDF') { return('.nc')
	} else if (format == 'KML') { return('.kml')
	} else if (format == 'KMZ') { return('.kmz')
#	} else if (format == 'BIG.MATRIX') { return('.big')
	} else if (format == 'BIL') { return('.bil')
	} else if (format == 'BSQ') { return('.bsq')
	} else if (format == 'BIP') { return('.bip')
	} else if (format == 'ASCII') { return('.asc')
	} else if (format == 'RST') { return('.rst') 
	} else if (format == 'ILWIS') { return('.mpr')
	} else if (format == 'SAGA') { return('.sdat')
	} else if (format == 'BMP') { return('.bmp') 
	} else if (format == 'ADRG') { return('.gen') 
	} else if (format == 'BT') { return('.bt') 
	} else if (format == 'EHdr') { return('.bil')
	} else if (format == 'ENVI') { return('.envi')
	} else if (format == 'ERS') { return('.ers') 
	} else if (format == 'GSBG') { return('.grd')
	} else if (format == 'HFA') { return( '.img') 
	} else if (format == 'IDA') { return( '.img') 
	} else if (format == 'RMF') { return('.rsw')
	} else { return('') }
}


