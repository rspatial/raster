# Author: Robert J. Hijmans
# Date : September 2017
# Version 1.0
# Licence GPL v3

.sf2sp <- function(from) {
	#if (!requireNamespace("sf")) {
	#	stop('package sf is not available')
	#}
	# to do
	#if (from == 'GEOMETRYCOLLECTION') {
	#	x <- list()
	#	for (i in 1:3 ) { 		}
	#	return(x)
	#}
	p <- as(from, 'Spatial')	
	if (isTRUE(ncol(p) == 0)) {
	# for the degerate Spatial*DataFrame that has zero variables
		if (inherits(p, 'SpatialPolygons')) {
			p <- as(p, 'SpatialPolygons')	
		} else if (inherits(p, 'SpatialLines')) {
			p <- as(p, 'SpatialLines')	
		} else if (inherits(p, 'SpatialPoints')) {
			p <- as(p, 'SpatialPoints')	
		}
	}
	p
}

