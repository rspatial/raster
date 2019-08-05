

setMethod('geom', signature(x='SpatialPolygons'), 
	function(x, sepNA=FALSE, ...) {
		
		nobs <- length(x@polygons)
		objlist <- list()
		cnt <- 0
		if (sepNA) {
			sep <- rep(NA,5)
			for (i in 1:nobs) {
				nsubobs <- length(x@polygons[[i]]@Polygons)
				ps <- lapply(1:nsubobs, 
						function(j)
							rbind(cbind(j, j+cnt, x@polygons[[i]]@Polygons[[j]]@hole, x@polygons[[i]]@Polygons[[j]]@coords), sep)
						)
				objlist[[i]] <- cbind(i, do.call(rbind, ps))
				cnt <- cnt+nsubobs
			}
		} else {
			for (i in 1:nobs) {
				nsubobs <- length(x@polygons[[i]]@Polygons)
				ps <- lapply(1:nsubobs, 
						function(j) 
							cbind(j, j+cnt, x@polygons[[i]]@Polygons[[j]]@hole, x@polygons[[i]]@Polygons[[j]]@coords)
						)
				objlist[[i]] <- cbind(i, do.call(rbind, ps))
				cnt <- cnt+nsubobs
			}
		}
		
		obs <- do.call(rbind, objlist)
		colnames(obs) <- c('object', 'part', 'cump', 'hole', 'x', 'y')
		rownames(obs) <- NULL
		
		if (sepNA) {
			obs[is.na(obs[,2]), ] <- NA
		}
		return( obs )
	}
)



setMethod('geom', signature(x='SpatialLines'), 
	function(x, sepNA=FALSE, ...) {
			
		nobs <- length(x@lines)
		objlist <- list()
		cnt <- 0
		if (sepNA) {
			sep <- rep(NA, 4)
			for (i in 1:nobs) {
				nsubobj <- length(x@lines[[i]]@Lines)
				ps <- lapply(1:nsubobj, 
						function(j) 
							rbind(cbind(j, j+cnt, x@lines[[i]]@Lines[[j]]@coords), sep)
						)
				objlist[[i]] <- cbind(i, do.call(rbind, ps))
				cnt <- cnt+nsubobj
			}
		} else {
			for (i in 1:nobs) {
				nsubobj <- length(x@lines[[i]]@Lines)
				ps <- lapply(1:nsubobj, function(j) cbind(j, j+cnt, x@lines[[i]]@Lines[[j]]@coords))
				objlist[[i]] <- cbind(i, do.call(rbind, ps))
				cnt <- cnt+nsubobj
			}
		}
		obs <- do.call(rbind, objlist)
		colnames(obs) <- c('object', 'part', 'cump', 'x', 'y')
		rownames(obs) <- NULL

		if (sepNA) {
			obs[is.na(obs[,2]), ] <- NA
		}
		return (obs)
	}
)


setMethod('geom', signature(x='SpatialPoints'), 
	function(x, ...) {
		xy <- coordinates(x)[,1:2,drop=FALSE]
		xy <- cbind(1:nrow(xy), xy)
		colnames(xy) <- c('object', 'x', 'y')
		return(xy)
	}
)
		

setMethod("geom", signature(x="data.frame"), 
	function(x, d, gt, crs, ...) {	
		if (gt == "polygons") {
			sp <- as(x, "SpatialPolygons")
			if (NROW(d) > 0) {
				sp <- sp::SpatialPolygonsDataFrame(sp, d)
			}
		} else if (gt == "lines") {
			sp <- as(x, "SpatialLines")		
			if (NROW(d) > 0) {
				sp <- sp::SpatialLinesDataFrame(sp, d)
			}
		} else {
			sp <- sp::SpatialPoints(x[,c("x", "y")])
			if (NROW(d) > 0) {
				sp <- sp::SpatialPointsDataFrame(sp, d)
			}
		}
		crs(sp)<- crs
		sp
	}
)
