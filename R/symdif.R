# Author: Robert J. Hijmans
# Date: December 2011
# Version 1.0
# Licence GPL v3


setMethod('symdif', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y, ...) {


	# warning("this method will be removed. You can use 'terra::symdif<SpatVector,SpatVector>' instead")

	z <- symdif(vect(x), vect(y))
	return(as(z, "Spatial"))

	# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))

	# haswarned <- FALSE

	# yy <- list(y, ...)
	# for (y in yy) {
		# if (! identical( .proj4string(x), .proj4string(y)) ) {
			# if (!haswarned) {
				# warning('non identical crs')
				# haswarned <- TRUE
			# }
			# y@proj4string <- x@proj4string
		# }
		# if (rgeos::gIntersects(x, y)) {
			# part1 <- erase(x, y)
			# part2 <- erase(y, x)
			# x <- bind(part1, part2)
		# }
	# }
	# x
}
)

