


.destair <- function(x, keepExtent=TRUE) {

	pts <- data.frame(geom(as(x, 'SpatialPolygons')))
	
	if (keepExtent) {
		bb <- bbox(x)
		ptsx1 <- pts[,5] == bb[1,1] 
		ptsx2 <- pts[,5] == bb[1,2] 
		ptsy1 <- pts[,6] == bb[2,1] 
		ptsy2 <- pts[,6] == bb[2,2] 
	}
	
	u <- unique(pts$cump)
	for (j in u) {
		k <- pts$cump==j
		p <- pts[k, 5:6]
		p <- rbind(p[(nrow(p)-1), ,drop=FALSE], p, p[2,,drop=FALSE])
		dx <- diff(p$x)
		dy <- diff(p$y)
		tf1 <- rowSums( cbind(dx[-length(dx)], dy[-1]) )
		tf2 <- rowSums( cbind(dx[-1], dy[-length(dy)]) )
		i <- which(tf1==0 | tf2==0) + 1
		p[i, ] <- (p[i-1, ] + p[i+1, ] + 2 * p[i, ]) / 4
		pts[k, 5:6] <- p[-c(1, nrow(p)),]
	}

	if (keepExtent) {
		pts[ptsx1,5] <- bb[1,1]
		pts[ptsx2,5] <- bb[1,2]
		pts[ptsy1,6] <- bb[2,1]
		pts[ptsy2,6] <- bb[2,2]
	}
	
	r <- as(pts, 'SpatialPolygons')
	row.names(r) <- row.names(x)
	crs(r) <- .getCRS(x)
	
	if (.hasSlot(x, 'data')) {
		r <- SpatialPolygonsDataFrame(r, x@data)
	}
	
	r
}


