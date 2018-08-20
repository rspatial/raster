# Author: Robert J. Hijmans
# Date : September 2012
# Version 1.0
# Licence GPL v3

.sf2sp <- function(from) {
	if (!requireNamespace("sf")) {
		warning('Cannot do this because sf is not available')
	}
	# to do
	#if (from == 'GEOMETRYCOLLECTION') {
	#	x <- list()
	#	for (i in 1:3 ) { 		}
	#	return(x)
	#}
	as(from, 'Spatial')	
}
