# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.writeAscii <- function(x, filename, datatype='FLT4S', prj=FALSE, ...) {

	v <- getValues(x)

	if (!is.finite( x@file@nodatavalue) ) {
		x@file@nodatavalue <- min(-9999, min(v, na.rm=TRUE)-1)
	}

	x <- .startAsciiWriting(x, filename, ...)
	
	datatype <- substr(datatype, 1, 3)
	if (datatype == 'INT') {
		on.exit(options(scipen=options('scipen')))
		options(scipen=10)
		v <- round(v)
	}

	v[is.na(v)] <- x@file@nodatavalue
	if (datatype=='FLT') {
		# hack to make sure that ArcGIS does not 
		# assume values are integers if the first 
		# values have no decimal point
		v <- as.character(v)
		v[1] <- formatC(as.numeric(v[1]), 15, format='f')
	}
	v <- matrix(v, ncol=ncol(x), byrow=TRUE)

	utils::write.table(v, x@file@name, append = TRUE, quote = FALSE, sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)

	if (prj) {
		crs <- .getSRS(x)
		if (!is.na(crs)) {
			writeLines(wkt(x), extension(filename, 'prj') )
		}
	}	
	
	return( .stopAsciiWriting(x) )
	
}
 
 