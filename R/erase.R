
	

.gDif <- function(x, y, type='polygons') {
	xln <- length(x)
	yln <- length(y)
	if (xln==0 | yln==0) {
		return(x)
	}
	rn <- row.names(x)
	for (i in xln:1) {
		z <- x[i,]
		for (j in 1:yln) {
			z <- rgeos::gDifference(z, y[j,], drop_lower_td=TRUE)
			if (is.null(z)) {
				break
			}
		}
		if (is.null(z)) {
			x <- x[-i,]
			rn <- rn[-i]
		} else {
			if (type=='polygons') {
				x@polygons[i] <- z@polygons
			} else {
				x@lines[i] <- z@lines	
			}
		}
		if (length(rn) > 0) {
			row.names(x) <- rn				
		}
	}
	if ((type=='polygons') & (length(x) > 0)) {

		w <- getOption('warn')
		on.exit(options('warn' = w))
		options('warn'=-1) 

		j <- rgeos::gIsValid(x, byid=TRUE, reason=FALSE)
		#j <- which(gArea(x, byid=TRUE) > 0)			
		if (!all(j)) {
			bad <- which(!j)
			for (i in bad) {
				# it could be that a part of a polygon is a sliver, but that other parts are OK
				a <- disaggregate(x[i, ])
				if (length(a) > 1) {
					jj <- which(rgeos::gIsValid(a, byid=TRUE, reason=FALSE))
					a <- a[jj, ]
					if (length(a) > 0) {
						x@polygons[i] <- aggregate(a)@polygons
						j[i] <- TRUE
					}
				} 
			}
			x <- x[j,]
			rn <- rn[j]			
		}
	
		if (length(rn) > 0) {
			row.names(x) <- rn
		}
	}
	x
}


setMethod(erase, signature(x='SpatialPolygons', y='SpatialPolygons'),
    function(x, y, ...){ 
	
		valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))

		prj <- x@proj4string
		if (is.na(prj)) prj <- y@proj4string
		x@proj4string <- sp::CRS(as.character(NA))
		y@proj4string <- sp::CRS(as.character(NA))

		if (!.hasSlot(x, "data")) {
			d <- data.frame(erase_dissolve_ID=1:length(x))
			rownames(d) <- row.names(x)
			x <- sp::SpatialPolygonsDataFrame(x, data=d)
			dropframe <- TRUE
		} else {
			dropframe <- FALSE
			x$erase_dissolve_ID <- 1:nrow(x)
		}

		y <- aggregate(y)
		
		int <- rgeos::gIntersects(x, y, byid=TRUE)
		int1 <- apply(int, 2, any)
		int2 <- apply(int, 1, any)
				
		if (sum(int1) == 0) { # no intersections
			return(x)
		}
		
		if (all(int1)) {
			part1 <- NULL
		} else {
			part1 <- x[!int1,]
		}
		part2 <- .gDif(x[int1,], y[int2,])

		part2 <- sp::SpatialPolygonsDataFrame(part2, x@data[match(row.names(part2), rownames(x@data)), ,drop=FALSE])
		if (!is.null(part1)) {
			part2 <- rbind(part1, part2)
		}
			
		if (length(part2@polygons) > 1) {	
			part2 <- aggregate(part2, colnames(part2@data))
		}
		part2@proj4string <- prj

		if (dropframe) {
			return( as(part2, 'SpatialPolygons') )
		} else {
			part2@data$erase_dissolve_ID <- NULL
			return( part2 )
		}
	}
)

setMethod(erase, signature(x='SpatialLines', y='SpatialPolygons'),
    function(x, y, ...){ 
	
		valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
		prj <- x@proj4string
		if (is.na(prj)) prj <- y@proj4string
		x@proj4string <- sp::CRS(as.character(NA))
		y@proj4string <- sp::CRS(as.character(NA))

		
		if (!.hasSlot(x, 'data')) {
			d <- data.frame(ID=1:length(x))
			rownames(d) <- row.names(x)
			x <- sp::SpatialLinesDataFrame(x, data=d)
			dropframe <- TRUE
		} else {
			dropframe <- FALSE
		}

		y <- aggregate(y)
		
		int <- rgeos::gIntersects(x, y, byid=TRUE)
		int1 <- apply(int, 2, any)
		int2 <- apply(int, 1, any)		

		if (sum(int1) == 0) { # no intersections
			return(x)
		}
		
		if (all(int1)) {
			part1 <- NULL
		} else {
			part1 <- x[!int1,]
		}
		part2 <- .gDif(x[int1,], y[int2,], 'lines')

		part2 <- sp::SpatialLinesDataFrame(part2, x@data[match(row.names(part2), rownames(x@data)), ,drop=FALSE], match.ID = FALSE)
		if (!is.null(part1)) {
			part2 <- rbind(part1, part2)
		}
			
		if (length(part2@lines) > 1) {	
			part2 <- aggregate(part2, colnames(part2@data))
		}
		
		part2@proj4string <- prj
		if (dropframe) {
			return( as(part2, 'SpatialLines') )
		} else {
			return( part2 )
		}		

	}
)

