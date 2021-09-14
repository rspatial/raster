# Author: Robert J. Hijmans
# Date : April 2015
# Version 1.0
# Licence GPL v3


# easy functions for creating SpatialLines* and SpatialPolygons*

spLines <- function(x, ..., attr=NULL, crs="") {
	x <- c(list(x), list(...))
	x <- rapply(x, Line, how='replace')
	x <- lapply(1:length(x), function(i) Lines(x[[i]], as.character(i)))
	x <- sp::SpatialLines(x)

	if (!is.null(attr)) {
		if (nrow(attr) == length(x)) {
			x <- sp::SpatialLinesDataFrame(x, attr)
		} else {
			msg <- paste('number of rows in attr (', nrow(attr), ') does not match the number of lines (', length(x), ')', sep='')
			stop(msg)
		}
	}

	if (!is.na(crs)) {
		crs(x) <- crs
	}
	x
}


spPolygons <- function(x, ..., attr=NULL, crs="") {
	x <- c(list(x), list(...))
	x <- rapply(x, Polygon, how='replace')

	x <- lapply(1:length(x), function(i) {
				if (length(x[[i]]) == 1) {
					Polygons(x[i], as.character(i))
				} else {
					Polygons(x[[i]], as.character(i))
				}
			})
	
	x <- sp::SpatialPolygons(x)
	if (!is.null(attr)) {
		if (nrow(attr) == length(x)) {
			x <- sp::SpatialPolygonsDataFrame(x, attr)
		} else {
			msg <- paste('number of rows in attr (', nrow(attr), ') does not match the number of polygons (', length(x), ')', sep='')
			stop(msg)
		}
	}
	
	if (!is.na(crs)) {
		crs(x) <- crs
	}
	
	x
}
