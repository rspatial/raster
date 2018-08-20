# Author: Robert J. Hijmans
# Date : July 2011
# Version 0.9
# Licence GPL v3


# Note: these functions create a _r_aster object (small r) (grDevices) for use with the rasterImage function
# _NOT_ a Raster* object as defined in this package

	
if (!isGeneric("as.raster")) {
	setGeneric("as.raster", function(x, ...)
		standardGeneric("as.raster"))
}	



setMethod('as.raster', signature(x='RasterLayer'), 
function(x, maxpixels=50000, col=rev(terrain.colors(255)), ...) {
	x <- as.matrix(sampleRegular(x, maxpixels, asRaster=TRUE))
	r <- range(x, na.rm=TRUE)
	x <- (x - r[1])/ (r[2] - r[1])
	x <- round(x * (length(col)-1) + 1)
	x[] <- col[x]
	as.raster(x)
} )


#e <- as.vector(t(bbox(extent(r))))
#a <- as.raster(r)
#plot(e[1:2], e[3:4], type = "n", xlab="", ylab="")
#graphics::rasterImage(a, e[1], e[3], e[2], e[4])
 
