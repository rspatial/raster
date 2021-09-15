# R function for the raster package
# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3


drawPoly <- function(sp=TRUE, col='red', lwd=2, ...) {
	xy <- graphics::locator(n=10000, type="l", col=col, lwd=lwd, ...)
	xy <- cbind(xy$x, xy$y)
	xy <- rbind(xy, xy[1,])
	lines(xy[(length(xy[,1])-1):length(xy[,1]),], col=col, lwd=lwd, ...)
	if (sp) {
		return( sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(xy)), 1))) )
	} else {
		return(xy)
	}
}


drawLine <- function(sp=TRUE, col='red', lwd=2, ...) {
	xy <- graphics::locator(n=10000, type="l", col=col, lwd=lwd, ...)
	xy <- cbind(xy$x, xy$y)
	if (sp) {
		return( sp::SpatialLines(list(sp::Lines(list(sp::Line(xy)), "1"))) )
	} else {
		return(xy)
	}
}

