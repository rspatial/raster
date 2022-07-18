# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3



setMethod('crop', signature(x='Spatial', y='ANY'), 
	function(x, y, ...) {

		# warning("this method will be removed. You can use 'terra::crop<SpatVector>' instead")
		if (inherits(y, "Extent")) {
			y = ext(y)
		} else {
			y <- extent(y)
			methods::validObject(y)
			y <- as(y, 'SpatialPolygons')
			y = vect(y)
		}
		x <- vect(x)
		z <- crop(x, y)
		return(as(z, "Spatial"))
	
	
		# if (! inherits(y, 'SpatialPolygons')) {
			# if (inherits(y, 'Extent')) {
				# y <- as(y, 'SpatialPolygons')
			# } else { 
				# y <- extent(y)
				# methods::validObject(y)
				# y <- as(y, 'SpatialPolygons')
			# }
			# y@proj4string <- x@proj4string		
		# }

		# prj <- x@proj4string
		# if (is.na(prj)) prj <- y@proj4string
		# x@proj4string <- sp::CRS(as.character(NA))
		# y@proj4string <- sp::CRS(as.character(NA))

		# if (inherits(y, 'SpatialPolygons')) {
			# y <- rgeos::gUnaryUnion(y)
			# row.names(y) <- '1'
			# y <- sp::geometry(y)
		# }
				
		# if (inherits(x, 'SpatialPolygons')) {
			# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
			# x <- .cropSpatialPolygons(x, y, ...)
		# } else if (inherits(x, 'SpatialLines')) {
			# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
			# x <- .cropSpatialLines(x, y, ...)
		# } else if (inherits(x, 'SpatialPoints')) {
			# x <- .cropSpatialPoints(x, y, ...)
		# } else {
			# x <- x[y]
		# }
		# if (inherits(x, "Spatial")) { x@proj4string <- prj }
		# x
	}
)	


# .cropSpatialPolygons <- function(x, y, ...) {
	
		# rnx <- row.names(x)
		# row.names(x) <- as.character(1:length(rnx))
		
		# if (.hasSlot(x, 'data')) {
			
			# # to keep the correct IDs
			# # in future versions of rgeos, this intermediate step won't be necessary
			# i <- as.vector( rgeos::gIntersects(x, y, byid=TRUE) )
			# if (sum(i) == 0) {
				# return(NULL)
			# }
			# y <- rgeos::gIntersection(x[i,], y, byid=TRUE, drop_lower_td=TRUE)
			# if (inherits(y, "SpatialCollections")) {
				# y <- y@polyobj
			# }
			# if (is.null(y)) { return(y) }
			
			# ids <- strsplit(row.names(y), ' ') 
			# ids <- as.numeric(do.call(rbind, ids)[,1])
			# row.names(y) <- as.character(rnx[ids])
			# data <- x@data[ids, ,drop=FALSE]
			# rownames(data) <- rnx[ids]
			# return( sp::SpatialPolygonsDataFrame(y, data) )
		# } else {
			# y <- rgeos::gIntersection(x, y, drop_lower_td=TRUE)
			# #if (inherits(y, "SpatialCollections")) {
			# #	y <- y@polyobj
			# #}
			# return(y)
		# }
# }


# .cropSpatialLines <- function(x, y, ...) {
	
	# rnx <- row.names(x)
	# row.names(x) <- as.character(1:length(rnx))

	# xy <- rgeos::gIntersection(x, y, byid=TRUE)
	# if (inherits(xy, "SpatialCollections")) {
		# xy <- xy@lineobj
	# }

	# if (.hasSlot(x, 'data')) {
					
		# ids <- strsplit(row.names(xy), ' ') 
		# ids <- as.numeric(do.call(rbind, ids)[,1])
		# #row.names(y) <- as.character(rnx[ids])
		# data <- x@data[ids, ,drop=FALSE]
		# #rownames(data) <- rnx[ids]
			
		# xy <- sp::SpatialLinesDataFrame(xy, data, match.ID = FALSE)
	# } 
	# return(xy)
# }



# .cropSpatialPoints <- function(x, y, ...) {

	# i <- which(!is.na(sp::over(x, y)))
	# if (length(i) > 0) {
		# x <- x[i,]
	# } else {
		# x <- NULL
	# }
	# x
	
# }


