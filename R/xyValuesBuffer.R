# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.9
# Licence GPL v3


.xyvBuf <- function(object, xy, buffer, fun=NULL, na.rm=TRUE, layer, nl, cellnumbers=FALSE, small=FALSE, onlycells=FALSE) { 

	buffer <- abs(buffer)
	if (length(buffer == 1)) {
		buffer <- rep(buffer, times=nrow(xy))
	} else if (length(buffer) != nrow(xy)  | ! is.vector(buffer) ) {
		stop('buffer should be a single value or a vector of length==nrow(xy)')
	}
	buffer[is.na(buffer)] <- 0

	if (onlycells) {
		cellnumbers <- TRUE
		fun <- NULL
		small <- TRUE
		object <- raster(object)
	} else if (! is.null(fun)) { 
		cellnumbers <- FALSE 
	}
	
	cv <- list()
	obj <- raster(object) 
# ?	centralcells <- cellFromXY(obj, xy)
	if (couldBeLonLat(obj)) { 
		# from m to degrees
		bufy <- buffer / 111319.5
		ymx <- pmin(90, xy[,2] + bufy)
		ymn <- pmax(-90, xy[,2] - bufy)
		bufx1 <- buffer / pointDistance(cbind(0, ymx), cbind(1, ymx), lonlat=TRUE)
		bufx2 <- buffer / pointDistance(cbind(0, ymn), cbind(1, ymn), lonlat=TRUE)
		bufx <- pmax(bufx1, bufx2)

		cn <- colFromX(obj, xy[,1]-bufx)
		cx <- colFromX(obj, xy[,1]+bufx)
		cn[is.na(cn) &  (xy[,1]-bufx <= xmin(obj) & xy[,1]+bufx >= xmin(obj))] <- 1
		cx[is.na(cx) &  (xy[,1]-bufx <= xmax(obj) & xy[,1]+bufx > xmax(obj))] <- ncol(obj)
		rn <- rowFromY(obj, xy[,2]+bufy)
		rx <- rowFromY(obj, xy[,2]-bufy)
		rn[is.na(rn) &  (xy[,2]-bufy <= ymax(obj) & xy[,2]+bufy >= ymax(obj))] <- 1
		rx[is.na(rx) &  (xy[,2]-bufy <= ymin(obj) & xy[,2]+bufy >= ymin(obj))] <- nrow(obj)

		for (i in 1:nrow(xy)) {
			s <- sum(rn[i], rx[i], cn[i], cx[i])
			if (is.na(s)) {
				cv[[i]] <- NA
			} else {
				if (onlycells) {
					value <- i
				} else {
					value <- getValuesBlock(object, rn[i], rx[i]-rn[i]+1, cn[i], cx[i]-cn[i]+1)
				}
				cell <- cellFromRowColCombine(obj, rn[i]:rx[i], cn[i]:cx[i])
				coords <- xyFromCell(obj, cell)
				if (cellnumbers) {
					pd <- cbind(pointDistance(xy[i,], coords, lonlat=TRUE), cell, value)
				} else {
					pd <- cbind(pointDistance(xy[i,], coords, lonlat=TRUE), value)
				}
				if (nrow(pd) > 1) {
					v <- pd[pd[,1] <= buffer[i], -1]
					if (NROW(v) == 0) {
						cv[[i]] <- pd[which.min(pd[,1]), -1]
					} else {
						cv[[i]] <- v
					}
				} else { 
					cv[[i]] <- pd[,-1]
				}
			}
		}
		
	} else { 

		cn <- colFromX(obj, xy[,1]-buffer)
		cx <- colFromX(obj, xy[,1]+buffer)
		cn[is.na(cn) &  (xy[,1]-buffer <= xmin(obj) & xy[,1]+buffer >= xmin(obj))] <- 1
		cx[is.na(cx) &  (xy[,1]-buffer <= xmax(obj) & xy[,1]+buffer > xmax(obj))] <- ncol(obj)
		rn <- rowFromY(obj, xy[,2]+buffer)
		rx <- rowFromY(obj, xy[,2]-buffer)
		rn[is.na(rn) &  (xy[,2]-buffer <= ymax(obj) & xy[,2]+buffer >= ymax(obj))] <- 1
		rx[is.na(rx) &  (xy[,2]-buffer <= ymin(obj) & xy[,2]+buffer >= ymin(obj))] <- nrow(obj)


		if (.doCluster()) {
			cl <- getCluster()
			on.exit( returnCluster() )
			nodes <- min(nrow(xy), length(cl))
			message('Using cluster with ', nodes, ' nodes')
			utils::flush.console()

	
			parallel::clusterExport(cl, c('object', 'obj', 'cellnumbers'), envir=environment())
			
			clFun2 <- function(i, xy, rn, rx, cn, cx) {
				s <- sum(rn, rx, cn, cx)
				if (is.na(s)) {
					return(NA)
				} else {
					if (onlycells) {
						value <- i
					} else {
						value <- getValuesBlock(object, rn, rx-rn+1, cn, cx-cn+1)
					}
					cell <- cellFromRowColCombine(obj, rn:rx, cn:cx)
					coords <- xyFromCell(obj, cell)
					if (cellnumbers) {
						pd <- cbind(pointDistance(xy, coords, lonlat=TRUE), cell, value)
					} else {
						pd <- cbind(pointDistance(xy, coords, lonlat=TRUE), value)
					}
					if (nrow(pd) > 1) {
						pd <- pd[pd[,1] <= buffer[i], -1]
					} else { 
						pd <- pd[,-1]
					}					
					return(pd)
				}
			}
			.sendCall <- eval( parse( text="parallel:::sendCall") )
			for (i in 1:nodes) {
				.sendCall(cl[[i]], clFun2, list(i, xy[i, ,drop=FALSE], rn[i], rx[i], cn[i], cx[i]), tag=i)
			}
			for (i in 1:nrow(xy)) {
				d <- .recvOneData(cl)
				if (! d$value$success) {
					print(d)
					stop('cluster error')
				} else {
					cv[[i]] <- d$value$value
				}
				ni <- nodes + i
				if (ni <= nrow(xy)) {
					.sendCall(cl[[d$node]], clFun2, list(ni, xy[i, ,drop=FALSE], rn[i], rx[i], cn[i], cx[i]), tag=i)
				}
			}
		} else {
			for (i in 1:nrow(xy)) {
				s <- sum(rn[i], rx[i], cn[i], cx[i])
				if (is.na(s)) {
					cv[[i]] <- NA
				} else {
					if (onlycells) {
						value <- i
					} else {
						value <- getValuesBlock(object, rn[i], rx[i]-rn[i]+1, cn[i], cx[i]-cn[i]+1)
					}
					cell <- cellFromRowColCombine(obj, rn[i]:rx[i], cn[i]:cx[i])
					coords <- xyFromCell(obj, cell)
					if (cellnumbers) {
						pd <- cbind(pointDistance(xy[i,], coords, lonlat=FALSE), cell, value)
					} else {
						pd <- cbind(pointDistance(xy[i,], coords, lonlat=FALSE), value)
					}
					if (nrow(pd) > 1) {
						cv[[i]] <- pd[pd[,1] <= buffer[i], -1]
					} else { 
						cv[[i]] <- pd[,-1]
					}
				}
			}
		}
	}

	if (small) {
		i <- sapply(cv, function(x) length(x)==0)
		if (any(i)) { 
			i <- which(i)
			if (onlycells) {
				vv <- cbind(cellFromXY(object, xy[i, ,drop=FALSE]), NA)
			} else {
				vv <- extract(object, xy[i, ,drop=FALSE], na.rm=na.rm, layer=layer, nl=nl, cellnumbers=cellnumbers)
			}
			if (NCOL(vv) > 1) {
				for (j in 1:length(i)) {
					cv[[ i[j] ]] <- vv[j, ]
				}			
			} else {
				for (j in 1:length(i)) {
					cv[[ i[j] ]] <- vv[j]
				}
			}
		}
	}
	
	nls <- nlayers(object)
	nms <- names(object)
	if (nls > 1) {
		if (layer > 1 | nl < nls) {
			lyrs <- layer:(layer+nl-1) 
			nms <- nms[ lyrs ]
			cv <- lapply(cv, function(x) x[, lyrs ])
		}
	}
	
	if (! is.null(fun)) {
		if (na.rm) {
			fun2 <- function(x){
						x <- stats::na.omit(x)
						if (length(x) > 0) { return(fun(x)) 
						} else { return(NA) 
						}
					}
		} else {
			fun2 <- fun
		}
		if (inherits(object, 'RasterLayer')) {
			cv <- unlist(lapply(cv, fun2), use.names = FALSE)
		} else {
			np <- length(cv)
			cv <- lapply(cv, function(x) {
				if (!is.matrix(x)) { x <- t(matrix(x)) }
				apply(x, 2, fun2)}
			)
			cv <- matrix(unlist(cv, use.names = FALSE), nrow=np, byrow=TRUE)
			colnames(cv) <- nms
		}
	}
	return(cv)
}
 

 
 
