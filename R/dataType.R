# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3




'dataType<-' <- function(x, value) {
	if (inherits(x, 'RasterStack')) {
		stop('Cannot set datatype of a RasterStack')
	}

# for backward compatibility issues and non fatal mistakes.
	datatype <- substr( toupper( trim(value) ), 1, 5)
	if (datatype == 'LOGIC') {datatype <- 'LOG1S'
	} else if (datatype == 'BYTE') {datatype <- 'INT1U'
	} else if (datatype == 'SMALL') {datatype <- 'INT2S'
	} else if (datatype == 'INTEG') {datatype <- 'INT2S'
	} else if (datatype == 'NUMER') {datatype <- 'FLT4S'
	} else if (datatype == 'FLOAT') {datatype <- 'FLT4S'
	} else if (datatype == 'DOUBL') {datatype <- 'FLT8S'
	} else if (datatype == 'SINGL') {datatype <- 'FLT4S'
	} else if (datatype == 'REAL') {datatype <- 'FLT4S'}	
	
	if (nchar(datatype) < 3) {
		stop(paste('invalid datatype:', datatype))
	} else if (nchar(datatype) == 3) {
		if (datatype == 'LOG') { 
			datatype <- paste(datatype, '1S', sep='') 		
		} else {
			datatype <- paste(datatype, '4S', sep='') 
		}
	} else if (nchar(datatype) == 4) {
		if (datatype == 'INT1') { 
			datatype <- paste(datatype, 'U', sep='') 
		} else { 
			datatype <- paste(datatype, 'S', sep='')
		}
	}

# now for real
	
	if (!(substr(datatype, 1, 4) %in% c('LOG1', 'INT1', 'INT2', 'INT4', 'FLT4', 'FLT8'))) {
		stop('not a valid data type')
	}
	type <- substr(datatype,1,3)
	size <- substr(datatype,4,4)
	signed <- substr(datatype,5,5) != 'U'
	
	if (type == "FLT") {
#		if (dataContent(x) != 'nodata') { 
#			x@data@values[] <- as.numeric(x@data@values)
#		}
		if (size == '4') {
			x@file@datanotation <- 'FLT4S'
			x@file@nodatavalue <- -3.4E38
		} else if (size == '8') {
			x@file@datanotation <- 'FLT8S'
			x@file@nodatavalue <-  -1.7E308
		} else { 
			stop("invalid datasize for a FLT (should be 4 or 8)") 
		}
	} else if (type == "INT") {
#		x@data@min <- round(x@data@min)
#		x@data@max <- round(x@data@max)
#		if (dataContent(x) != 'nodata') { 
#				x@data@values[] <- as.integer(round(x@data@values))
#			}                  
#		}
		
		if (size == '4') {
			if (signed) {
				x@file@datanotation <- 'INT4S'
				x@file@nodatavalue <- -2147483647
			} else {
				x@file@datanotation <- 'INT4U'
				x@file@nodatavalue <- 4294967295
			}
		} else if (size == '2') {
			if (signed) {
				x@file@datanotation <- 'INT2S'
				x@file@nodatavalue <- -32768
			} else {
				x@file@datanotation <- 'INT2U'
				x@file@nodatavalue <- 65535
			}
		} else if (size == '1') {
			if (signed) {
				x@file@datanotation <- 'INT1S'
				x@file@nodatavalue <- as.double(NA)  # no default NA value
			} else {
				x@file@datanotation <- 'INT1U'
				x@file@nodatavalue <- as.double(NA)  # no default NA value
			}
#		} else if (size == '8') {
#			x@file@nodatavalue <- -9223372036854775808
#			x@file@datanotation <- 'INT8S'							
		} else {
			stop("invalid datasize for this datatype") 
		}
	} else if ( type == 'LOG' ) {
		x@file@nodatavalue <- -128
		x@file@datanotation <- 'LOG1S'
	} else {
		stop("unknown datatype")
	} 
	return(x)
}

