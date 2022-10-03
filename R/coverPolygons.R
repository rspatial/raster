# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3


setMethod('cover', signature(x='SpatialPolygons', y='SpatialPolygons'), 
	function(x, y, ..., identity=FALSE){ 

	# warning("this method will be removed. You can use 'terra::cover<SpatVector,SpatVector>' instead")

	# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
	
	# prj <- x@proj4string
	# if (is.na(prj)) prj <- y@proj4string
	# x@proj4string <- sp::CRS(as.character(NA))
	
	yy <- list(y, ...)

	i <- which(sapply(yy, function(x) inherits(x, 'SpatialPolygons')))
	if (length(i)==0) {
		stop('additional arguments should be of class SpatialPolygons')
	} else if (length(i) < length(yy)) {
		warning('additional arguments that are not of class SpatialPolygons are ignored')
		yy <- yy[i]
	} 

	x <- vect(x)
	for (y in yy) {
		x <- cover(x, vect(y), identity=identity, expand=FALSE)
	}
	x
	
	# if (identity) {
		# x <- .coverIdentity(x, yy)
		# if (inherits(x, "Spatial")) { x@proj4string <- prj }
		# return(x)
	# }
	
	# for (y in yy) {
		# y@proj4string <- sp::CRS(as.character(NA))
		# subs <- rgeos::gIntersects(x, y, byid=TRUE)
		# if (!any(subs)) {
			# next
		# } else {
			# int <- crop(y, x)
			# x <- erase(x, int)
			# x <- bind(x, int)
		# }
	# }
	# x@proj4string <- prj
	# x
} 
)




# .coverIdentity <- function(x, yy) {

	# for (y in yy) {
		# y@proj4string <- sp::CRS(as.character(NA))
		# i <- rgeos::gIntersects(x, y)
		# if (!i) {
			# next
		# }
	
		# x <- sp::spChFIDs(x, as.character(1:length(x)))
		# y <- sp::spChFIDs(y, as.character(1:length(y)))

		# if (.hasSlot(x, 'data')) {
			# xnames <- colnames(x@data)
		# } else {
			# xnames <-NULL
		# }
		# if (.hasSlot(y, 'data')) {
			# ynames <- colnames(y@data)
		# } else {
			# ynames <-NULL
		# }
		# if (is.null(xnames) & !is.null(ynames)) {
			# dat <- y@data[NULL, ,drop=FALSE]
			# dat[1:length(x), ] <- NA
			# x <- sp::SpatialPolygonsDataFrame(x, dat)
			# xnames <- ynames
		# }
		
		# yinx <- which(ynames %in% xnames)
		# doAtt <- TRUE
		# if (length(yinx) == 0) {
			# doAtt <- FALSE
		# }
		
		# subs <- rgeos::gIntersects(x, y, byid=TRUE)
		# subsx <- apply(subs, 2, any)
		# subsy <- apply(subs, 1, any)
	
		# int  <- rgeos::gIntersection(x[subsx,], y[subsy,], byid=TRUE, drop_lower_td=TRUE)
		# #if (inherits(int, "SpatialCollections")) {
		# #	if (is.null(int@polyobj)) { # ??
		# #		warning('polygons do not intersect')
		# #		next
		# #	}
		# #	int <- int@polyobj
		# #}
		# if (!inherits(int, 'SpatialPolygons')) {
			# warning('polygons do not intersect')
			# next
		# }

		# if (doAtt) {
			# ids <- do.call(rbind, strsplit(row.names(int), ' '))
			# idsy <- match(ids[,2], rownames(y@data))
			# rows <- 1:length(idsy)
			
			# dat <- x@data[NULL, ,drop=FALSE]
			# dat[rows, yinx] <- y@data[idsy, yinx]
			# int <- sp::SpatialPolygonsDataFrame(int, dat, match.ID=FALSE)
		# }
		# x <- erase(x, int)
		# if (is.null(x)) {
			# x <- int
		# } else {
			# x <- bind(x, int)
		# }
	# }
	# x
# } 




