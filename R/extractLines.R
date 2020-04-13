# Author: Robert J. Hijmans
# Date : December 2009
# Version 1.0
# Licence GPL v3


setMethod('extract', signature(x='Raster', y='SpatialLines'), 
function(x, y, fun=NULL, na.rm=FALSE, cellnumbers=FALSE, df=FALSE, layer, nl, factors=FALSE, along=FALSE, sp=FALSE, ...){ 

	px <- projection(x, asText=FALSE)
	comp <- compareCRS(px, projection(y), unknown=TRUE)
	if (!comp) {
		.requireRgdal()
		warning('Transforming SpatialLines to the CRS of the Raster object')
		y <- spTransform(y, px)
	}
	if (missing(layer)) {
		layer <- 1
	}
	if (missing(nl)) {
		nl <- nlayers(x)
	}	

	if (!is.null(fun)) {
		cellnumbers <- FALSE
		along <- FALSE
		if (sp) {
			df <- TRUE
		}
	} else {
		if (sp) {
			sp <- FALSE
			warning('argument sp=TRUE is ignored if fun=NULL')
		}
	}
	
	if (along) {
		return(.extractLinesAlong(x, y, cellnumbers=cellnumbers, df=df, layer, nl, factors=factors, ...))
	}

	spbb <- bbox(y)
	rsbb <- bbox(x)
	addres <- 2 * max(res(x))
	nlns <- length( y@lines )
	res <- list()
	res[[nlns+1]] <- NA
	
	
	if (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) {
		if (df) {
			res <- matrix(ncol=1, nrow=0)
			colnames(res) <- 'ID'
			return(res)
		} else {
			return(res[1:nlns])
		}
	}
	
	rr <- raster(x)
	cn <- names(x)
	pb <- pbCreate(nlns, label='extract', ...)
	
	if (.doCluster()) {
		.sendCall <- eval( parse( text="parallel:::sendCall") )

		cl <- getCluster()
		on.exit( returnCluster() )
		nodes <- min(nlns, length(cl)) 
		message('Using cluster with ', nodes, ' nodes')
		utils::flush.console()

		parallel::clusterExport(cl, c('rsbb', 'rr', 'addres', 'cellnumbers'), envir=environment())
		clFun <- function(i, pp) {
			spbb <- bbox(pp)
			if (! (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) ) {
				rc <- crop(rr, extent(pp)+addres)
				rc <- .linesToRaster(pp, rc, silent=TRUE)
				xy <- rasterToPoints(rc)[,-3,drop=FALSE]
				if (length(xy) > 0) { # always TRUE?
					r <- .xyValues(x, xy, layer=layer, nl=nl)
					if (cellnumbers) {
						r <- cbind(cellFromXY(rr, xy), r)
						colnames(r) <- c('cell', cn)
					}
				} else {
					r <- NULL
				}
			}
			r
		}
		
        for (ni in 1:nodes) {
			.sendCall(cl[[ni]], clFun, list(ni, y[ni,]), tag=ni)
		}
		
		for (i in 1:nlns) {
			d <- .recvOneData(cl)
			if (! d$value$success) {
				stop('cluster error at polygon: ', i)
			}
			res[[d$value$tag]] <- d$value$value
			ni <- ni + 1
			if (ni <= nlns) {
				.sendCall(cl[[d$node]], clFun, list(ni, y[ni,]), tag=ni)
			}
			pbStep(pb)
		}	
	
	
	} else {
	
	
		for (i in 1:nlns) {
			pp <- y[i,]
			spbb <- bbox(pp)
			if (! (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) ) {
				rc <- crop(rr, extent(pp)+addres)
				rc <- .linesToRaster(pp, rc, silent=TRUE)
				xy <- rasterToPoints(rc)[,-3,drop=FALSE]
				if (cellnumbers) {
					v <- cbind(cellFromXY(rr, xy), .xyValues(x, xy, layer=layer, nl=nl))
					colnames(v) <- c('cell', cn)
					res[[i]] <- v
				} else {
					res[[i]] <- .xyValues(x, xy, layer=layer, nl=nl)
				}
			} 
			pbStep(pb)
		}
	}
	
	res <- res[1:nlns]
	
	pbClose(pb)
	
	if (! is.null(fun)) {
		i <- sapply(res, is.null)
		if (nlayers(x) > 1) {
			j <- matrix(ncol=nlayers(x), nrow=length(res))
			j[!i] <- t(sapply(res[!i], function(x) apply(x, 2, fun, na.rm=na.rm)))
			colnames(j) <- names(x)
		} else {
			j <- vector(length=length(i))
			j[i] <- NA
			j[!i] <- sapply(res[!i], fun, na.rm=na.rm)
		}
		res <- j
	}
	
	if (df) {
		if (!is.list(res)) {
			res <- data.frame(ID=1:NROW(res), res)
		} else {
			res <- data.frame( do.call(rbind, sapply(1:length(res), function(x) if (!is.null(res[[x]])) cbind(x, res[[x]]))) )
		}		

		lyrs <- layer:(layer+nl-1)
		if (cellnumbers) {
			colnames(res) <- c("ID", "cell", names(x)[lyrs])
		} else {
			colnames(res) <- c("ID", names(x)[lyrs])		
		}
		
		if (any(is.factor(x)) & factors) {
			v <- res[, -1, drop=FALSE]
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v[,1], layer))
			} else {
				v <- .insertFacts(x, v, lyrs)
			}
			res <- data.frame(res[,1,drop=FALSE], v)
		}
	}
	
	if (sp) {
		if (nrow(res) != nlns) {
			warning('sp=TRUE is ignored because fun does not summarize the values of each line to a single number')
			return(res)
		}
	
		if (!.hasSlot(y, 'data') ) {
			y <- SpatialLinesDataFrame(y,  res[, -1, drop=FALSE], match.ID=FALSE)
		} else {
			y@data <- cbind(y@data,  res[, -1, drop=FALSE])
		}
		return(y)
	}
	res
}
)


.extractLinesAlong <- function(x, y, cellnumbers=FALSE, df=FALSE, layer, nl, factors=FALSE, ...){ 

	spbb <- bbox(y)
	rsbb <- bbox(x)
	addres <- 2 * max(res(x))
	nlns <- length( y@lines )
	res <- list()
	res[[nlns+1]] <- NA

	if (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) {
		if (df) {
			res <- matrix(ncol=1, nrow=0)
			colnames(res) <- 'ID'
			return(res)
		} else {
			return(res[1:nlns])
		}
	}
	
	rr <- raster(x)
	cn <- names(x)
	
	pb <- pbCreate(nlns, label='extract', ...)
	
	y <- data.frame(geom(y)	)
	for (i in 1:nlns) {
		yp <- y[y$object == i, ]
		nparts <- max(yp$part)
		vv <- NULL
		for (j in 1:nparts) {
			pp <- yp[yp$part==j, c('x', 'y'), ]
			for (k in 1:(nrow(pp)-1)) {
				ppp <- pp[k:(k+1), ]
				spbb <- bbox(as.matrix(ppp))
				if (! (spbb[1,1] > rsbb[1,2] | spbb[1,2] < rsbb[1,1] | spbb[2,1] > rsbb[2,2] | spbb[2,2] < rsbb[2,1]) ) {
					lns <- SpatialLines(list(Lines(list(Line(ppp)), "1")))
					rc <- crop(rr, extent(lns) + addres)
					rc <- .linesToRaster(lns, rc, silent=TRUE)
					xy <- rasterToPoints(rc)[,-3,drop=FALSE]
					v <- cbind(row=rowFromY(rr, xy[,2]), col=colFromX(rr, xy[,1]), .xyValues(x, xy, layer=layer, nl=nl))
					#up or down?
					
					updown <- c(1,-1)[(ppp[1,2] < ppp[2,2]) + 1]
					rightleft <- c(-1,1)[(ppp[1,1] < ppp[2,1]) + 1]

					v <- v[order(updown*v[,1], rightleft*v[,2]), ]

					#up <- ppp[1,2] < ppp[2,2]
					#right <- ppp[1,1] < ppp[2,1]					
#					if (up) {
#						if (right) {
#							v <- v[order(-v[,1], v[,2]), ]
#						} else {
#							v <- v[order(-v[,1], -v[,2]), ]
#						}
					
#					} else {
#						if (!right) {
#							v <- v[order(v[,1], -v[,2]), ]
#						}
#					}
					vv <- rbind(vv, v)
				}
			} 
			if (cellnumbers) {
				vv <- cbind(cellFromRowCol(rr, vv[,1], vv[,2]), vv[,-c(1:2)])
				colnames(vv) <- c('cell', names(x))
			} else {
				vv <- vv[,-c(1:2)]
				if (NCOL(vv) > 1) {
					colnames(vv) <- names(x)
				}
			}
			res[[i]] <- vv
			pbStep(pb)
		}
	}
	
	res <- res[1:nlns]
	pbClose(pb)
	
	
	if (df) {
		res <- data.frame( do.call(rbind, lapply(1:length(res), function(x) if (!is.null(res[[x]])) cbind(x, res[[x]]))) )
		lyrs <- layer:(layer+nl-1)
		colnames(res) <- c('ID', names(x)[lyrs])
		
		if (any(is.factor(x)) & factors) {
			v <- res[, -1, drop=FALSE]
			if (ncol(v) == 1) {
				v <- data.frame(factorValues(x, v[,1], layer))
			} else {
				v <- .insertFacts(x, v, lyrs)
			}
			res <- data.frame(res[,1,drop=FALSE], v)
		}
	}
	res
}


