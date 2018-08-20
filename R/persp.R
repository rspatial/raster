# Author: Robert J. Hijmans
# Date :  April 2009
# Version 0.9
# Licence GPL v3

if (!isGeneric("persp")) {
	setGeneric("persp", function(x,...)
		standardGeneric("persp"))
}	

setMethod("persp", signature(x='RasterLayer'), 
	function(x, maxpixels=100000, ext=NULL, ...)  {
		x <- sampleRegular(x, size=maxpixels, ext=ext, asRaster=TRUE, useGDAL=TRUE)
		value <- t((getValues(x, format='matrix'))[nrow(x):1,])
		y <- yFromRow(x, nrow(x):1)
		x <- xFromCol(x,1:ncol(x))
		persp(x=x, y=y, z=value, ...)
	}
)

setMethod("persp", signature(x='RasterStackBrick'), 
	function(x, y=1, maxpixels=10000, ext=NULL, ...)  {
		if (y < 1) { y <- 1 }
		if (y > nlayers(x)) { y <- nlayers(x) }
		x <- raster(x, y)
		persp(x=x, maxpixels=maxpixels, ext=ext, ...)
	}	
)

