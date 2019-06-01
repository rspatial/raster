# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.9
# Licence GPL v3


setMethod('unique', signature(x='RasterLayer', incomparables='missing'), 
function(x, incomparables=FALSE, na.last=NA, progress="", ...) {
	
	if (! inMemory(x) ) {
		if ( fromDisk(x) ) {
			if (canProcessInMemory(x, 2)) {
				x <- readAll(x)
			}
		} else {
			stop('RasterLayer has no values')	
		}
	} 

	if ( inMemory(x) ) {
		x <- unique(x@data@values, incomparables=incomparables, progress="", ...)
		return(sort(x, na.last=na.last))
	} else {
		u1 <- vector()
		u2 <- vector()
		
		tr <- blockSize(x, n=2)
		pb <- pbCreate(tr$n, label='unique', progress=progress, ...)	
		for (i in 1:tr$n) {
			u1 <- unique( c(u1, getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i])), incomparables=incomparables, ... )
			if (length(u1) > 10000 ) {
				u2 <- unique(c(u1, u2), incomparables=incomparables, ...)
				u1 <- vector()
			}
			pbStep(pb, i)			
		}
		pbClose(pb)
		return(sort(unique(c(u1, u2), incomparables=incomparables, ...), na.last=na.last))	
	}
}
)


setMethod('unique', signature(x='RasterStackBrick', incomparables='missing'), 
function(x, incomparables=FALSE, na.last=NA, progress="", ...) {
	
	
	if (! inMemory(x) ) {
		if (canProcessInMemory(x, 2)) {
			x <- readAll(x)
		} 
	} 

	if ( inMemory(x) ) {
	
		x <- unique(getValues(x), incomparables=incomparables, ...)
		if (is.list(x)) {
			for (i in 1:length(x)) {
				x[[i]] <- sort(x[[i]], na.last=na.last)
			}
		} else {
			xx <- vector(length=ncol(x), mode='list')
			for (i in 1:ncol(x)) {
				xx[[i]] <- sort(x[,i], na.last=na.last)
			}
			x <- xx
		}
		return(x)
		
	} else {
		nl <- nlayers(x)
		un <- list(length=nl, mode='list')
		tr <- blockSize(x, n=2)
		pb <- pbCreate(tr$n, label='unique', progress=progress)
		un <- NULL
		for (i in 1:tr$n) {
			v <- unique( getValues(x, row=tr$row[i], nrows=tr$nrows[i]) )
			un <- unique(rbind(v, un), incomparables=incomparables, ...)
			pbStep(pb, i)			
		}
		pbClose(pb)
		return(un)	
	}
}
)

