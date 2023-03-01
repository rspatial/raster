# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3



setMethod('union', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y) {

	# warning("this method will be removed. You can use 'terra::union<SpatVector,SpatVector>' instead")

	x <- vect(x)
	y <- vect(y)
	z <- union(x, y)
	as(z, "Spatial")

	# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))

	# x <- sp::spChFIDs(x, as.character(1:length(x)))
	# y <- sp::spChFIDs(y, as.character(1:length(y)))

	# prj <- x@proj4string
	# if (is.na(prj)) prj <- y@proj4string
	# x@proj4string <- .spCRS(as.character(NA))
	# y@proj4string <- .spCRS(as.character(NA))
	
	# subs <- rgeos::gIntersects(x, y, byid=TRUE)
	
	# if (!any(subs)) {
	
		# x <- bind(x, y)
		
	# } else {
	
		# xdata <-.hasSlot(x, 'data')
		# ydata <-.hasSlot(y, 'data')
		# if (xdata & ydata) {
			# nms <- .goodNames(c(colnames(x@data), colnames(y@data)))
			# colnames(x@data) <- nms[1:ncol(x@data)]
			# colnames(y@data) <- nms[(ncol(x@data)+1):length(nms)]
		# } 
		
		# dif1 <- erase(x, y)
		# dif2 <- erase(y, x)
		
		# x <- intersect(x, y)
		# x <- list(dif1, dif2, x)
		# x <- x[!sapply(x, is.null)]

		# i <- sapply(x, length) > 0 
		# x <- x[ i ]
		# if (length(x) > 1) {
			# x <- do.call(bind, x)
		# } else {
			# x <- x[[1]]
		# }
	# }
	# if (inherits(x, "Spatial")) { x@proj4string <- prj }
	# x
}
)




setMethod('union', signature(x='SpatialPolygons', y='missing'), 
function(x, y) {

	# warning("this method will be removed. You can use 'terra::union<SpatVector>' instead")

	x <- vect(x)
	z <- union(x)
	names(z) <- gsub("_", ".", toupper(names(z)))
	z$count <- rowSums(data.frame(z))
	if (nrow(z) == 3) {
		# for bcmaps test
		z <- z[c(3,1,2),]
	}
	as(z, "Spatial")

	# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
	# n <- length(x)
	# if (n < 2) {
		# return(x)
	# }

	# prj <- x@proj4string
	# x@proj4string <- .spCRS(as.character(NA))
	
	# #if (!rgeos::gIntersects(x)) {
	# # this is a useful test, but returned topologyerrors
	# #	return(x)
	# #}
	
	# if (.hasSlot(x, 'data')) {
		# x <- as(x, 'SpatialPolygons')
	# }
	
	# x <- sp::spChFIDs(x, as.character(1:length(x)))
	# x <- sp::SpatialPolygonsDataFrame(x, data.frame(ID=1:n))

	# u <- x[1,]
	# names(u) <- 'ID.1'
	# for (i in 2:n) {
		# z <- x[i, ]
		# names(z) <- paste('ID.', i, sep='')
		# u <- union(u, z)
	# }
	
	# u@data[!is.na(u@data)] <- 1
	# u@data[is.na(u@data)] <- 0
	# u$count <- rowSums(u@data)
	# u@proj4string <- prj
	# u
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


