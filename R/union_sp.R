# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3

if (!isGeneric("union")) {
	setGeneric("union", function(x, y)
		standardGeneric("union"))
}	


setMethod('union', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y) {

	stopifnot(requireNamespace("rgeos"))

	x <- spChFIDs(x, as.character(1:length(x)))
	y <- spChFIDs(y, as.character(1:length(y)))

	if (! identical(proj4string(x), proj4string(y)) ) {
		warning('non identical CRS')
		y@proj4string <- x@proj4string
	}
	
	subs <- rgeos::gIntersects(x, y, byid=TRUE)
	
	if (!any(subs)) {
	
		x <- bind(x, y)
		
	} else {
	
		xdata <-.hasSlot(x, 'data')
		ydata <-.hasSlot(y, 'data')
		if (xdata & ydata) {
			nms <- .goodNames(c(colnames(x@data), colnames(y@data)))
			colnames(x@data) <- nms[1:ncol(x@data)]
			colnames(y@data) <- nms[(ncol(x@data)+1):length(nms)]
		} 
		
		dif1 <- erase(x, y)
		dif2 <- erase(y, x)
		
		x <- intersect(x, y)
		x <- list(dif1, dif2, x)
		x <- x[!sapply(x, is.null)]

		i <- sapply(x, length) > 0 
		x <- x[ i ]
		if (length(x) > 1) {
			x <- do.call(bind, x)
		} else {
			x <- x[[1]]
		}
	}

	x
}
)




setMethod('union', signature(x='SpatialPolygons', y='missing'), 
function(x, y) {

	stopifnot(requireNamespace("rgeos"))
	n <- length(x)
	if (n < 2) {
		return(x)
	}
	
	#if (!rgeos::gIntersects(x)) {
	# this is a useful test, but returned topologyerrors
	#	return(x)
	#}
	
	if (.hasSlot(x, 'data')) {
		x <- as(x, 'SpatialPolygons')
	}
	
	x <- spChFIDs(x, as.character(1:length(x)))
	x <- SpatialPolygonsDataFrame(x, data.frame(ID=1:n))

	u <- x[1,]
	names(u) <- 'ID.1'
	for (i in 2:n) {
		z <- x[i, ]
		names(z) <- paste('ID.', i, sep='')
		u <- union(u, z)
	}
	
	u@data[!is.na(u@data)] <- 1
	u@data[is.na(u@data)] <- 0
	u$count <- rowSums(u@data)
	
	u
}	
)

setMethod('union', signature(x='SpatialPoints', y='SpatialPoints'), 
function(x, y) {
	bind(x,y)
})

setMethod('union', signature(x='SpatialLines', y='SpatialLines'), 
function(x, y) {
	bind(x,y)
})


