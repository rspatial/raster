# Author: Robert J. Hijmans
# Date :  June 2016
# Version 0.1
# Licence GPL v3


.kernelDensity <- function(xy, r, bandwidth) {
	requireNamespace("MASS")
    lims <- as.vector(extent(r)) + rep(res(r), each=2) * c(0.5,-0.5)
    n <- rev(dim(r)[1:2])
    xy <- .pointsToMatrix(xy)
	k <- raster( MASS::kde2d(xy[,1], xy[,2], h=bandwidth, n=n, lims=lims) )
	# to avoid possible small changes due to floating point math and to transfer CRS
	setValues(r, getValues(k)) 
}

