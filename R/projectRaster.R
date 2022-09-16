# Author: Robert J. Hijmans
# Date :  January 2009
# Version 1.0
# Licence GPL v3

#.strCRS <- function(object) {
#	p <- .getCRS(object)
#	p <- suppressWarnings(wkt(p))
#	if (is.null(p)) p <- projection(p)
#	p
#}


projectExtent <- function(object, crs) {
#	.requireRgdal()
#	use_proj6 <- .useproj6()
	
	object <- raster(object)
	dm <- oldm <- dim(object)
	# simple way to avoid a bug with a single column/row reported by 
	# Jon Olav Skoien
	dm[1] <- max(10, dm[1])
	dm[2] <- max(10, dm[2])
	dim(object) <- dm
	pfrom <- projection(object)
	pto <- crs

	rows <- unique(c(seq(1,nrow(object), by=max(1, round(nrow(object)/50))), nrow(object)))
	cols <- unique(c(seq(1,ncol(object), by=max(1, round(ncol(object)/50))), ncol(object)))
	
	xy1 <- xyFromCell(object, cellFromRowCol(object, rows, 1))
	xy1[,1] <- xy1[,1] - 0.5 * xres(object)
	xy1[1,2] <- xy1[1,2] + 0.5 * yres(object)
	xy1[nrow(xy1),2] <- xy1[nrow(xy1),2] + 0.5 * yres(object)
	
	xy2 <- xyFromCell(object, cellFromRowCol(object, rows, ncol(object)))
	xy2[,1] <- xy2[,1] + 0.5 * xres(object)
	xy2[1,2] <- xy2[1,2] + 0.5 * yres(object)
	xy2[nrow(xy2),2] <- xy2[nrow(xy2),2] + 0.5 * yres(object)

	xy3 <- xyFromCell(object, cellFromRowCol(object, 1, cols))
	xy3[,2] <- xy3[,2] + 0.5 * yres(object)
	xy3[1,1] <- xy3[1,1] - 0.5 * xres(object)
	xy3[ncol(xy3),1] <- xy3[ncol(xy3),1] + 0.5 * xres(object)
	
	xy4 <- xyFromCell(object, cellFromRowCol(object, nrow(object), cols))
	xy4[,2] <- xy4[,2] - 0.5 * yres(object)
	xy4[1,1] <- xy4[1,1] - 0.5 * xres(object)
	xy4[ncol(xy4),1] <- xy4[ncol(xy4),1] + 0.5 * xres(object)
	
	# added for circumpolar data:
	if (nrow(object) > 75 & ncol(object) > 75) {
		xy5 <- sampleRegular(object, 500, xy=TRUE)
#		rows <- c(seq(min(nrow(object), 25), nrow(object), by=50))
#		cols <- c(seq(min(ncol(object), 25), ncol(object), by=50))
#		xy5 <- xyFromCell(object, cellFromRowColCombine(object, rows, cols))
		xy <- rbind(xy1, xy2, xy3, xy4, xy5)
	} else {
		xy <- rbind(xy1, xy2, xy3, xy4)
	}
	
	xy <- terra::vect(xy, crs=pfrom)
	xy <- crds(terra::project(xy, pto))
	xy <- subset(xy, !(is.infinite(xy[,1]) | is.infinite(xy[,2])) )
	x <- xy[,1]
	y <- xy[,2]
	
	if (length(y) == 0 | length(y) ==0) { stop("cannot do this transformation") }
	minx <- min(x)
	maxx <- max(x)
	if (maxx == minx) {
		maxx <- maxx + 0.5
		minx <- minx - 0.5
	}
	miny <- min(y)
	maxy <- max(y)
	if (maxy == miny) {
		maxy <- maxy + 0.5
		miny <- miny - 0.5
	}
	
	obj <- raster(extent(minx, maxx, miny,  maxy), nrows=oldm[1], ncols=oldm[2], crs=crs)
	return(obj)
}


.computeRes <- function(obj, crs, ...) {

	x <- xmin(obj) + 0.5 * (xmax(obj) - xmin(obj))
	y <- ymin(obj) + 0.5 * (ymax(obj) - ymin(obj))
	res <- res(obj)
	x1 <- x - 0.5 * res[1]
	x2 <- x + 0.5 * res[1]
	y1 <- y - 0.5 * res[2]
	y2 <- y + 0.5 * res[2]
	xy <- cbind(c(x1, x2, x, x), c(y, y, y1, y2))
	
	v <- vect(xy, crs=projection(obj))
	pXY <- crds(project(v, crs))

#	pXY <- cbind(pXY[[1]], pXY[[2]])
#	out <- c((pXY[2,1] - pXY[1,1]), (pXY[4,2] - pXY[3,2]))
	outex <- extent(pXY)
	out <- c(xmax(outex) - xmin(outex),	ymax(outex) - ymin(outex))

	if (any(is.na(out))) {
		if (isLonLat(obj)) {
			out <- pointDistance(cbind(x1, y1), cbind(x2, y2), lonlat=TRUE)
			out <- c(out, out)
		} else {
			out <- res
		}
	}
	# abs should not be necessary, but who knows what a projection might do?
	abs( signif(out, digits=3) )
}


.getAlignedRaster <- function(x,y) {
	x <- raster(x)
	y <- raster(y)
	p <- projectRaster(x, crs=projection(y))
	m <- merge(extent(y), extent(p))
	rx <- extend(y, m)
	crop(rx, p)
}

if (!isGeneric("projectRaster")) {setGeneric("projectRaster", function(from, ...) standardGeneric("projectRaster"))}

setMethod('projectRaster', signature(from='Raster'), 
function(from, to, res, crs, method="bilinear", alignOnly=FALSE, over=FALSE, filename="", ...)  {


#	.requireRgdal()
#	use_proj6 <- .useproj6()

	projfrom <- projection(from)
	if (is.na(projfrom)) { 
		stop("input projection is NA") 
	}


	lonlat <- isLonLat(projfrom)
	if (missing(to)) {
		if (missing(crs)) {
			stop("both 'to' and 'crs' arguments are missing.")
		}
		projto <- crs
		projfrom <- projection(projfrom)
	
#		}
		to <- projectExtent(from, projto)
		crs(to) <- projto

		if (missing(res)) {
			res <- .computeRes(from, projto)
		}
		res(to) <- res

		# add some cells to capture curvature
		e <- extent(to)
		add <- min(5, min(dim(to)[1:2])/10) * max(res)
		e@ymin <- e@ymin - add
		e@ymax <- e@ymax + add
		e@xmin <- e@xmin - add
		e@xmax <- e@xmax + add
		if (!is.character(projto)) projto <- projto@projargs
		if (substr(projto, 1, 13) == "+proj=longlat") {
			e@xmin <- max(-180, e@xmin)
			e@xmax <- min(180, e@xmax)
			e@ymin <- max(-90, e@ymin)
			e@ymax <- min(90, e@ymax)
		}
		to <- extend(to, e)
	} else {
	
		projto <- projection(to)
		if (is.na(projto)) { 
			stop("output projection is NA") 
		} 
		#projfrom <-.strCRS(projfrom)
		
		e <- extent( projectExtent(from, projto) )
		add <- min(10, min(dim(to)[1:2])/10) * max(raster::res(to))
		e@ymin <- e@ymin - add
		e@ymax <- e@ymax + add
		e@xmin <- e@xmin - add
		e@xmax <- e@xmax + add
		if (isLonLat(projto)) {
			e@xmin <- max(-180, e@xmin)
			e@xmax <- min(180, e@xmax)
			e@ymin <- max(-90, e@ymin)
			e@ymax <- min(90, e@ymax)
		}

	}
	
	projto_int <- projto	

	if (alignOnly) {
		to <- .getAlignedRaster(from, to)
		return (to)
	}
	
	if (!method %in% c('bilinear', 'ngb')) { 
		stop('invalid method') 
	}

	nl <- nlayers(from)
	if ( nl == 1) {
		to <- raster(to)
		if (method=="ngb") { 
			colortable(to) <- colortable(from)
		}
	} else {
		to <- brick(to, values=FALSE, nl=nl)
	}

	if (method=='ngb') { 
		#method <- 'simple' # for extract (.xyValues)

		method = "near"

	} 


	names(to) <- names(from)
	if ( ! hasValues(from) ) {
		#warning("'from' has no cell values")
		return(to)
	}
	
	opt <- list(...)
	opt$progress = NULL
	p = project(rast(from), rast(to), method=method, filename=filename, gdal=FALSE, wopt=opt)
	return(as(p, "Raster"))

	
	if (canProcessInMemory(to, n=nl*4)) {
		inMemory <- TRUE
	} else {
		inMemory <- FALSE
	}

		
	if (.doCluster()) {
		
		cl <- getCluster()
		on.exit( returnCluster() )

		nodes <- min(ceiling(to@nrows/10), length(cl)) # at least 10 rows per node
		
		message('Using cluster with ', nodes, ' nodes')
		utils::flush.console()
		
		tr <- blockSize(to, minblocks=nodes)
		pb <- pbCreate(tr$n, label='projectRaster', ...)

		parallel::clusterExport(cl, c('tr', 'to', 'from', 'e', 'nl', 'projto_int', 'projfrom', 'method'), envir=environment())
		
		clFun <- function(i) {
			start <- cellFromRowCol(to, tr$row[i], 1)
			end <- start + tr$nrows[i] * ncol(to) - 1
			cells <- start:end
			xy <- xyFromCell(to, cells) 
			xy <- subset(xy, xy[,1] > e@xmin & xy[,1] < e@xmax)
			v <- matrix(nrow=length(cells), ncol=nl)
			if (nrow(xy) > 0) {
				ci <- match(cellFromXY(to, xy), cells)
				xy <- project(xy, projto_int, projfrom)			
				v[ci, ] <- .xyValues(from, xy, method=method)
			} 
			return(v)
		}
	
		.sendCall <- eval( parse( text="parallel:::sendCall") )	
		# for debugging
		# parallel::clusterExport(cl,c("tr", "projto", "projfrom", "method", "from", "to"))
        for (i in 1:nodes) {
			.sendCall(cl[[i]], clFun, list(i), tag=i)
		}
		        
		if (inMemory) {
			v <- matrix(nrow=ncell(to), ncol=nlayers(from))

			for (i in 1:tr$n) {
				pbStep(pb, i)
				d <- .recvOneData(cl)
				if (! d$value$success) {
					print(d)
					stop('cluster error')
				}
				start <- cellFromRowCol(to, tr$row[d$value$tag], 1)
				end <- start + tr$nrows[d$value$tag] * ncol(to) - 1
				v[start:end, ] <- d$value$value
				ni <- nodes+i
				if (ni <= tr$n) {
					.sendCall(cl[[d$node]], clFun, list(ni), tag=ni)
				}
			}
			
			to <- setValues(to, v)
			if (filename != '') {
				to <- writeRaster(to, filename, ...)
			}
			pbClose(pb)
			return(to)
			
		} else {
			to <- writeStart(to, filename=filename, ...)

			for (i in 1:tr$n) {
				pbStep(pb, i)
				d <- .recvOneData(cl)
				if (! d$value$success ) { 
					print(d)
					stop('cluster error') 
				}
				to <- writeValues(to, d$value$value, tr$row[d$value$tag])
				ni <- nodes+i
				if (ni <= tr$n) {
					.sendCall(cl[[d$node]], clFun, list(ni), tag=ni)
				}
			}
			pbClose(pb)
			to <- writeStop(to)	
			return(to)
		}	

		
	} else {
		# this seems to need smaller chunks
		#cz <- max(5, 0.1 * .chunk() / nlayers(to))
		
		
		if (inMemory) {
			
			xy <- sp::coordinates(to) 
			xy <- subset(xy, xy[,1] > e@xmin & xy[,1] < e@xmax)
			cells <- cellFromXY(to, xy)
			xy <- terra::project(xy, projto_int, projfrom)			
			to[cells] <- .xyValues(from, xy, method=method)
			
			if (filename != '') {
				to <- writeRaster(to, filename, ...)
			}	
			return(to)
			
		} else {
		
			tr <- blockSize(to, n=nlayers(to)*4)
			pb <- pbCreate(tr$n, label='projectRaster', ...)	
			to <- writeStart(to, filename=filename, ...)
			for (i in 1:tr$n) {
				cells <- cellFromRowCol(to, tr$row[i], 1):cellFromRowCol(to, tr$row[i]+tr$nrows[i]-1, ncol(to))
				xy <- xyFromCell(to, cells ) 
				xy <- subset(xy, xy[,1] > e@xmin & xy[,1] < e@xmax)
				if (nrow(xy) > 0) {
					ci <- match(cellFromXY(to, xy), cells)
					xy <- project(xy, projto_int, projfrom)			
					v <- matrix(nrow=length(cells), ncol=nl)
					v[ci, ] <- .xyValues(from, xy, method=method)
					to <- writeValues(to, v, tr$row[i])
				}	
				pbStep(pb)
			}
			pbClose(pb)
			to <- writeStop(to)	
			return(to)
		}
	}
}
)
