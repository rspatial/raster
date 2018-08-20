# Author: Robert J. Hijmans
# Date : November 2010
# Version 1.0
# Licence GPL v3


setMethod('as.array', signature(x='RasterLayer'), 
function(x, maxpixels, ...) {
	if (!hasValues(x)) { stop("'x' has no values") }
	if (! missing(maxpixels)) {
		x <- sampleRegular(x, maxpixels, asRaster=TRUE)
	}
	x <- array(as.matrix(x), c(dim(x)))
	x
} )

setMethod('as.array', signature(x='RasterStackBrick'), 
function(x, maxpixels, transpose=FALSE) {
	if (!hasValues(x)) { stop("'x' has no values") }
	if (! missing(maxpixels)) {
		x <- sampleRegular(x, maxpixels, asRaster=TRUE)
	}
	dm <- dim(x)
	x <- getValues(x)
	if (transpose) {
		ar <- array(NA, c(dm[2], dm[1], dm[3]))
		for (i in 1:dm[3]) {
			ar[,,i] <- matrix(x[,i], nrow=dm[2], byrow=FALSE)
		}	
	} else {
		ar <- array(NA, dm)
		for (i in 1:dm[3]) {
			ar[,,i] <- matrix(x[,i], nrow=dm[1], byrow=TRUE)
		}
	}
	ar
} )

