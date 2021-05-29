# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.9
# Licence GPL v3



setMethod('extract', signature(x='Raster', y='SpatialPolygons'), 
function(x, y, fun=NULL, na.rm=FALSE, exact=FALSE, normalizeWeights=TRUE, cellnumbers=FALSE, small=TRUE, df=FALSE, layer, nl, factors=FALSE, sp=FALSE, weights=FALSE, ...){ 

	#px <-.getCRS(x, asText=FALSE)
	px <-.getCRS(x)
	comp <- compareCRS(px,.getCRS(y), unknown=TRUE)
	if (!comp) {
		.requireRgdal()
		warning('Transforming SpatialPolygons to the CRS of the Raster')
		y <- spTransform(y, px)
	}
	
	spbb <- bbox(y)
	rsbb <- bbox(x)
	addres <- max(res(x))
	npol <- length(y@polygons)
	res <- list()
	res[[npol+1]] <- NA
	
	if (!is.null(fun)) {
		cellnumbers <- FALSE
	    if (weights || exact) {
			if (!is.null(fun)) {
				fun <- match.fun(fun)
				test <- try(methods::slot(fun, 'generic') == 'mean', silent=TRUE)
				if (!isTRUE(test)) {
					warning('"fun" was changed to "mean"; other functions cannot be used when "weights=TRUE"' )
				}
			}
			fun <- function(x, ...) {
				# some complexity here because different layers could 
				# have different NA cells
				if ( is.null(x) ) {
					return(rep(NA, nl))
				}
				w <- x[,nl+1]
				x <- x[,-(nl+1), drop=FALSE]
				x <- x * w
				w <- matrix(rep(w, nl), ncol=nl)
				w[is.na(x)] <- NA
				w <- colSums(w, na.rm=TRUE)
				x <- apply(x, 1, function(X) { X / w } )
				if (!is.null(dim(x))) {
					rowSums(x, na.rm=na.rm)
				} else {
					sum(x, na.rm=na.rm)
				}
			}
		}
		
		if (sp) {
			df <- TRUE
		}
		
		doFun <- TRUE
		
	} else {
		if (sp) {
			sp <- FALSE
			df <- FALSE
			warning('argument sp=TRUE is ignored if fun=NULL')
		#} else if (df) {
		#	df <- FALSE
		#	warning('argument df=TRUE is ignored if fun=NULL')
		}
		
		doFun <- FALSE
	}
	
	if (missing(layer)) {
		layer <- 1
	} else {
		layer <- max(min(nlayers(x), layer), 1)
	}
	if (missing(nl)) {
		nl <- nlayers(x) - layer + 1
	} else {
		nl <- max(min(nlayers(x)-layer+1, nl), 1)
	}
	
	
	if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
		if (df) {
			res <- data.frame(matrix(ncol=1, nrow=0))
			colnames(res) <- 'ID'
			return(res)
		}
		return(res[1:npol])
	}
	

	
	rr <- raster(x)
	
	pb <- pbCreate(npol, label='extract', ...)
	
	if (.doCluster()) {
		cl <- getCluster()
		on.exit( returnCluster() )
		nodes <- min(npol, length(cl)) 
		message('Using cluster with ', nodes, ' nodes')
		utils::flush.console()

		.sendCall <- eval( parse( text="parallel:::sendCall") )
		parallel::clusterExport(cl, c('rsbb', 'rr', 'weights', 'exact', 'addres', 'cellnumbers', 'small'), envir=environment())
		
		clFun <- function(i, pp) {
			spbb <- bbox(pp)
		
			if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
				# do nothing; res[[i]] <- NULL
			} else {
				rc <- crop(rr, extent(pp)+addres)
				if (weights) {
					rc <- .polygonsToRaster(pp, rc, getCover=TRUE, silent=TRUE)
					rc[rc==0] <- NA
					xy <- rasterToPoints(rc)
					if (normalizeWeights) {
						weight <- xy[,3] / sum(xy[,3])
					} else {
						weight <- xy[,3] #/ 100
					}
					xy <- xy[, -3, drop=FALSE]
				} else if (exact) {
					erc <- crop(x, rc)
					xy <- exactextractr::exact_extract(erc, pp, include_cell=cellnumbers, progress=FALSE)[[1]]
				} else {
					rc <- .polygonsToRaster(pp, rc, silent=TRUE)
					xy <- rasterToPoints(rc)[,-3,drop=FALSE]
				}
				
				if (length(xy) > 0) { # catch very small polygons
					if (exact) {
						r <- xy[,1]
						if (normalizeWeights) {
							xy$coverage_fraction <- xy$coverage_fraction / sum(xy$coverage_fraction)
						}
						if (cellnumbers) {
							cell <- cellFromXY(x, xy)
							r <- cbind(cell, r, xy$coverage_fraction)
						} else {				
							r <- cbind(r, xy$coverage_fraction)
						}
					} else {
						r <- .xyValues(x, xy, layer=layer, nl=nl)
						if (weights) {
							if (cellnumbers) {
								cell <- cellFromXY(x, xy)
								r <- cbind(cell, r, weight)
							} else {				
								r <- cbind(r, weight)
							}
						} else if (cellnumbers) {
							cell <- cellFromXY(x, xy)
							r <- cbind(cell, r)						
						}
					}
				} else {
					if (small) {
						ppp <- pp@polygons[[1]]@Polygons
						ishole <- sapply(ppp, function(z)z@hole)
						xy <- lapply(ppp, function(z)z@coords)
						xy <- xy[!ishole]
						if (length(xy) > 0) {
							cell <- unique(unlist(lapply(xy, function(z) cellFromXY(x, z)), use.names = FALSE))
							value <- .cellValues(x, cell, layer=layer, nl=nl)
							if (weights | exact) {
								weight=rep(1/NROW(value), NROW(value))
								if (cellnumbers) {
									r <- cbind(cell, value, weight)
								} else {
									r <- cbind(value, weight)								
								}
							} else if (cellnumbers) {
								r <- cbind(cell, value)					
							} else {
								r <- value
							}
						} else {
							r <- NULL
						}
					} else {
						r <- NULL
					}
				}
			}
			r
		}
		
        for (ni in 1:nodes) {
			.sendCall(cl[[ni]], clFun, list(ni, y[ni,]), tag=ni)
		}
		
		for (i in 1:npol) {
			d <- .recvOneData(cl)
			if (! d$value$success) {
				stop('cluster error at polygon: ', i)
			}

			if (doFun) {
				if (!is.null(d$value$value)) {
					if (nl > 1 & !(weights | exact)) {
						res[[d$value$tag]] <- apply(d$value$value, 2, fun, na.rm=na.rm)							
					} else { 
						res[[d$value$tag]] <- fun(d$value$value, na.rm=na.rm)
					}
				}
			} else {
				res[[d$value$tag]] <- d$value$value
			}
			ni <- ni + 1
			if (ni <= npol) {
				.sendCall(cl[[d$node]], clFun, list(ni, y[ni,]), tag=ni)
			}
			pbStep(pb, i)
		}
		
	} else {
		for (i in 1:npol) {
			pp <- y[i,]
			spbb <- bbox(pp)
		
			if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
				# do nothing; res[[i]] <- NULL
			} else {
				rc <- crop(rr, extent(pp)+addres)
				if (weights) {
					rc <- .polygonsToRaster(pp, rc, getCover=TRUE, silent=TRUE)
					rc[rc==0] <- NA
					xy <- rasterToPoints(rc)
					if (normalizeWeights) {
						weight <- xy[,3] / sum(xy[,3])
					} else {
						weight <- xy[,3] #/ 100			
					}
					xy <- xy[,-3,drop=FALSE]
				} else if (exact) {
					erc <- crop(x, rc)
					xy <- exactextractr::exact_extract(erc, pp, include_cell=cellnumbers, progress=FALSE)[[1]]	
				} else {
					rc <- .polygonsToRaster(pp, rc, silent=TRUE)
					xy <- rasterToPoints(rc)[,-3,drop=FALSE]
				}
			
				if (length(xy) > 0)  {  # catch holes or very small polygons
					if (weights) {
						value <- .xyValues(x, xy, layer=layer, nl=nl)
						if (cellnumbers) {
							res[[i]] <- cbind(cell, value, weight)
						} else {				
							res[[i]] <- cbind(value, weight)
						}
					} else if (exact) {
						if (normalizeWeights) {
							xy$coverage_fraction <- xy$coverage_fraction / sum(xy$coverage_fraction)
						}					
						colnames(xy)[ncol(xy)] <- "weight"
						res[[i]] <- as.matrix(xy)
					} else if (cellnumbers) {
						value <- .xyValues(x, xy, layer=layer, nl=nl)
						cell <- cellFromXY(x, xy)
						res[[i]] <- cbind(cell, value)		
					} else {
						res[[i]] <- .xyValues(x, xy, layer=layer, nl=nl)
					}
				} else if (small) {
					ppp <- pp@polygons[[1]]@Polygons
					ishole <- sapply(ppp, function(z)z@hole)
					xy <- lapply(ppp, function(z)z@coords)
					xy <- xy[!ishole]
					if (length(xy) > 0) {
						cell <- unique(unlist(lapply(xy, function(z) cellFromXY(x, z))), use.names = FALSE)
						value <- .cellValues(x, cell, layer=layer, nl=nl)
						if (weights | exact) {
							weight <- rep(1/NROW(value), NROW(value))
							if (cellnumbers) {
								res[[i]] <- cbind(cell, value, weight)
							} else {
								res[[i]] <- cbind(value, weight)
							}
						} else if (cellnumbers) {
							res[[i]] <- cbind(cell, value)					
						} else {
							res[[i]] <- value
						}
					} # else do nothing; res[[i]] <- NULL
				} 
				if (doFun) {
					if (!is.null(res[[i]])) {
						if (nl > 1 & !(weights | exact)) {
							res[[i]] <- apply(res[[i]], 2, fun, na.rm=na.rm)							
						} else {
							res[[i]] <- fun(res[[i]], na.rm=na.rm)
						}
					}
				}	
			}
			pbStep(pb)
		}
	}
	res <- res[1:npol]
	pbClose(pb)

	
	if (! is.null(fun)) {
		# try to simplify
		i <- sapply(res, length)
		if (length(unique(i[i != 0])) == 1) {
			if (any(i == 0)) {
				lng <- length(res)
				v <- do.call(rbind, res)
				res <- matrix(NA, nrow=lng, ncol=ncol(v))
				res[which(i > 0), ] <- v
			} else {
				res <- do.call(rbind, res)
			}
		} else {
			if (sp) {
				warning('cannot return a sp object because the data length varies between polygons')
				sp <- FALSE
				df <- FALSE
			#} else if (df) {
				#warning('cannot return a data.frame because the data length varies between polygons')
				#df <- FALSE
			}
		}
	}
	
	if (df) {
		if (!is.list(res)) {
			res <- data.frame(ID=1:NROW(res), res)
		} else {
			res <- data.frame( do.call(rbind, lapply(1:length(res), function(x) if (!is.null(res[[x]])) cbind(x, res[[x]]))) )
		}		

		lyrs <- layer:(layer+nl-1)
		if (cellnumbers) {
			nms <- c('ID', 'cell', names(x)[lyrs])
		} else {
			nms <- c('ID', names(x)[lyrs])
		}
		if ((weights|exact) & is.null(fun)) {
			nms <- c(nms, 'weight')
		}
		colnames(res) <- nms
		
		if (any(is.factor(x)) & factors) {
			i <- ifelse(cellnumbers, 1:2, 1)
			v <- res[, -i, drop=FALSE]
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v[,1], layer))
			} else {
				v <- .insertFacts(x, v, lyrs)
			}
			res <- data.frame(res[,i,drop=FALSE], v)
		}
	}
	
	if (sp) {
		if (nrow(res) != npol) {
			warning('sp=TRUE is ignored because fun does not summarize the values of each polygon to a single number')
			return(res)
		}
		
		if (!.hasSlot(y, 'data') ) {
			y <- SpatialPolygonsDataFrame(y, res[, -1, drop=FALSE])
		} else {
			y@data <- cbind(y@data, res[, -1, drop=FALSE])
		}
		return(y)
	}

	res
}
)

