# Author: Robert J. Hijmans
# Date : June 2008
# Version 0.9
# Licence GPL v3



stackOpen <- function(stackfile) {
	f <- utils::read.table(stackfile, as.is=FALSE, strip.white=TRUE)
	if (dim(f)[2] > 1) {
		s <- stack(as.vector(f[,1]), bands=as.vector(f[,2]))
	} else {
		s <- stack(as.vector(f[,1]))
	}
	s@filename <- stackfile
	return(s)
}

..stackOpen <- function(stackfile, quick=FALSE) {
	f <- utils::read.table(stackfile, as.is=FALSE, strip.white=TRUE)
	if (quick) {
		if (dim(f)[2] > 1) {
			s <- .quickStack(f[,1], f[,2], f[,3])	
		} else {
			s <- .quickStack(f[,1])
		}
	} else {
		if (dim(f)[2] > 1) {
			s <- stack(as.vector(f[,1]), bands=as.vector(f[,2]))
		} else {
			s <- stack(as.vector(f[,1]))
		}
	}
	s@filename <- stackfile
	return(s)
}

stackSave <- function(x, filename) {
	filename <- trim(filename)
	if (filename == "") { 
		stop('Provide a non empty filename.') 
	}
	
	info <- t( sapply(x@layers, function(i) c(i@file@name, i@file@nbands, i@data@band)) )
	if (any(info[,1] == '')) {
		stop("cannot save a RasterStack that has layers that only exist in memory. Use writeRaster first/instead.")
	}
	if (any(info[,2] != '1')) {
		utils::write.table(info, filename, row.names=FALSE, col.names=FALSE)
	} else {
		utils::write.table(info[,1], filename, row.names=FALSE, col.names=FALSE)
	}
	x@filename <- filename
	return(x)
}

