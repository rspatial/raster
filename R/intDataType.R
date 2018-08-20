# raster package
# Author: Robert J. Hijmans
# Date :  November 2009
# Version 0.9
# Licence GPL v3


.checkIntDataType <- function(mn, mx, dtype) {
	mn <- round(mn)
	mx <- round(mx)
	ok <- TRUE
	if (dtype == 'INT') {
		return(.getIntDataType(mn, mx) )
	} else if (dtype == 'INT1S') {
		if (mn < -127 | mx > 128) {
			ok <- FALSE
		}
	} else if (dtype == 'INT1U') {
		if (mn < 0 | mx > 256) {
			ok <- FALSE		
		}
	} else  if (dtype == 'INT2S') {
		if (mn < -32767 | mx > 32768) {
			ok <- FALSE			
		}
	} else  if (dtype == 'INT2U') {
		if (mn <= 0 | mx > 65534 ) {
			ok <- FALSE
		}
	} else if (dtype == 'INT4S') {
		if (mn < -2147483647 | mx > 2147483648 ) {
			ok <- FALSE
		}
	} else if (dtype == 'INT4U') {
		if (mn < 0 | mx > 2^32 ) {
			ok <- FALSE
		}
#	} else if (dtype == 'INT8S') {
#		if (mn < -2^63/2 | mx > 2^64/2) {
#			ok <- FALSE
#		}
	} else {
		stop('unknown integer type:', dtype)
	}
	if (!ok) { 
		dtype <- .getIntDataType(mn, mx)
		warning('changed INT data type to: ', dtype)
	} 	
	return(dtype)
}


.getIntDataType <- function(mn, mx) {
# optimize the number of bytes within the datatype
	if (mn > -128 & mx < 128) {
		datatype <- 'INT1S'
	} else if (mn >=0 & mx < 256) {
		datatype <- 'INT1U'
	} else if (mn > -32767 & mx < 32768) {
		datatype <- 'INT2S'
	} else if (mn >= 0 & mx < 65534 ) {
		datatype <- 'INT2U'
	} else if (mn > -2147483647 & mx < 2147483648 ) {
		datatype <- 'INT4S'
	} else if (mn > 0 & mx < 2^32 ) {
		datatype <- 'INT4U'
##	} else if (mn > -(2^63/2) & mx < (2^64/2)) {
#		datatype <- 'INT8S'
	} else {
		stop('these values are too large to be saved as integers')
	}
	return(datatype)
}



..intSetNA <- function(v, dtype) {
	if (dtype == 'INT1S') {
		v[v < -127 | v > 128] <- NA
	} else if (dtype == 'INT1U') {
		v[v <=0 | v > 256] <- NA
	} else  if (dtype == 'INT2S') {
		v[v < -32767 | v > 32768] <- NA
	} else  if (dtype == 'INT2U') {
		v[v <= 0 | v > 65534] <- NA
	} else if (dtype == 'INT4S') {
		v[v < -2147483647 | v > 2147483648] <- NA
	} else if (dtype == 'INT8S') {
		v[v < -2^63/2 | v > 2^64/2] <- NA
	} 
	return(v)
}


