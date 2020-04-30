# Author: Robert J. Hijmans
# Date : June 2014
# Version 1.0
# Licence GPL v3


setMethod('extract', signature(x='SpatialPolygons', y='SpatialPoints'), 
function(x, y, ...){ 
	
	stopifnot(requireNamespace("rgeos"))
	
	if (! identical(.oldproj4string(x), .oldproj4string(y)) ) {
		warning('non identical CRS')
		y@proj4string <- x@proj4string
	}
    i <- rgeos::gIntersects(y, x, byid=TRUE)
	
	j <- cbind(1:length(y), rep(1:length(x), each=length(y)), as.vector(t(i)))
	j <- j[j[,3] == 1, -3, drop=FALSE]
	colnames(j) <- c('point.ID', 'poly.ID')
	if (.hasSlot(x, 'data')) {
		r <- data.frame(j, x@data[j[,2], ,drop=FALSE], row.names=NULL)
	} else {
		r <- data.frame(j, row.names=NULL)
	}
	q <- data.frame(point.ID = 1:length(y))
	merge(q, r, by='point.ID', all=TRUE)
})



setMethod('extract', signature(x='SpatialPolygons', y='data.frame'),
function(x, y, ...) { 
	stopifnot(ncol(y) == 2)
	y <- as.matrix(y)
	stopifnot(is.numeric(y[1,1]))
	extract(x, y, ...)
}
) 

setMethod('extract', signature(x='SpatialPolygons', y='matrix'), 
function(x, y, ...) { 
	stopifnot(ncol(y) == 2)
	stopifnot(is.numeric(y[1,1]))

	i <- which(rowSums(is.na(y)) == 0)
	if (length(i) == 0) {
		r <- cbind(data.frame(point.ID=1:nrow(y), poly.ID=NA), x@data[0,][1:nrow(y),])
		rownames(r) <- NULL
	} else if (length(i) < nrow(y)) {
		sp <- SpatialPoints(y[i,], proj4string=x@proj4string)
		v <- extract(x, sp, ...)
		r <- cbind(data.frame(point.ID=1:nrow(y), poly.ID=NA), x@data[0,][1:nrow(y),])
		if (nrow(v) == nrow(sp)) {  # no overlapping polygons
			r[i, ] <- v			
		} else {
			r <- r[! r$point.ID %in% i, ]
			r <- rbind(r, v)
			r <- r[order(r$point.ID), ]
		}
		rownames(r) <- NULL
	} else {
		sp <- SpatialPoints(y, proj4string=x@proj4string)
		r <- extract(x, sp, ...)
	}
	return(r)
}
)
