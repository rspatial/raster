# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3


setMethod('cover', signature(x='SpatialPolygons', y='SpatialPolygons'), 
	function(x, y, ..., identity=FALSE){ 
	
	stopifnot(requireNamespace("rgeos"))
	
	yy <- list(y, ...)

	i <- which(sapply(yy, function(x) inherits(x, 'SpatialPolygons')))
	if (length(i)==0) {
		stop('additional arguments should be of class SpatialPolygons')
	} else if (length(i) < length(yy)) {
		warning('additional arguments that are not of class SpatialPolygons are ignored')
		yy <- yy[i]
	} 

	if (identity) {
		return(.coverIdentity(x, yy))
	}
	
	haswarned <- FALSE
	for (y in yy) {
		if (! identical(.oldproj4string(x), .oldproj4string(y)) ) {
			if (!haswarned) {
				warning('non identical CRS')
				haswarned <- TRUE
			}
			y@proj4string <- x@proj4string
		}	
		subs <- rgeos::gIntersects(x, y, byid=TRUE)
		if (!any(subs)) {
			next
		} else {
			int <- crop(y, x)
			x <- erase(x, int)
			x <- bind(x, int)
		}
	}
	x
} 
)




.coverIdentity <- function(x, yy) {

	haswarned <- FALSE
	for (y in yy) {
		if (! identical(.oldproj4string(x), .oldproj4string(y)) ) {
			if (!haswarned) {
				warning('non identical CRS')
				haswarned <- TRUE
			}
			y@proj4string <- x@proj4string
		}	
		
		i <- rgeos::gIntersects(x, y)
		if (!i) {
			next
		}
	
		x <- spChFIDs(x, as.character(1:length(x)))
		y <- spChFIDs(y, as.character(1:length(y)))

		if (.hasSlot(x, 'data')) {
			xnames <- colnames(x@data)
		} else {
			xnames <-NULL
		}
		if (.hasSlot(y, 'data')) {
			ynames <- colnames(y@data)
		} else {
			ynames <-NULL
		}
		if (is.null(xnames) & !is.null(ynames)) {
			dat <- y@data[NULL, ,drop=FALSE]
			dat[1:length(x), ] <- NA
			x <- SpatialPolygonsDataFrame(x, dat)
			xnames <- ynames
		}
		
		yinx <- which(ynames %in% xnames)
		doAtt <- TRUE
		if (length(yinx) == 0) {
			doAtt <- FALSE
		}
		
		subs <- rgeos::gIntersects(x, y, byid=TRUE)
		subsx <- apply(subs, 2, any)
		subsy <- apply(subs, 1, any)
	
		int  <- rgeos::gIntersection(x[subsx,], y[subsy,], byid=TRUE, drop_lower_td=TRUE)
		#if (inherits(int, "SpatialCollections")) {
		#	if (is.null(int@polyobj)) { # ??
		#		warning('polygons do not intersect')
		#		next
		#	}
		#	int <- int@polyobj
		#}
		if (!inherits(int, 'SpatialPolygons')) {
			warning('polygons do not intersect')
			next
		}

		if (doAtt) {
			ids <- do.call(rbind, strsplit(row.names(int), ' '))
			idsy <- match(ids[,2], rownames(y@data))
			rows <- 1:length(idsy)
			
			dat <- x@data[NULL, ,drop=FALSE]
			dat[rows, yinx] <- y@data[idsy, yinx]
			int <- SpatialPolygonsDataFrame(int, dat, match.ID=FALSE)
		}
		x <- erase(x, int)
		if (is.null(x)) {
			x <- int
		} else {
			x <- bind(x, int)
		}
	}
	x
} 




