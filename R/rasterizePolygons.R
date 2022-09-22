# Author: Robert J. Hijmans
# Date : January 2009
# Version 2.0
# Licence GPL v3


.getPutVals <- function(obj, field, n, mask) {

	if (mask) {
		return( data.frame(v=rep(1, length=n)) )
	
	} else if (missing(field)) {
		if (.hasSlot(obj, 'data')) {
			putvals <- obj@data
			cn <- validNames(c('ID', colnames(putvals)))
			cn[1] <- 'ID'
			putvals <- data.frame(ID=1:nrow(putvals), putvals)
			colnames(putvals) <- cn	
		} else {
			putvals <- data.frame(v=as.integer(1:n))
		}
		return(putvals)
		
	} else if (isTRUE (is.na(field))) { 
		return( data.frame(v=rep(NA, n)) )

		
	} else if (is.character(field) ) {
		if (.hasSlot(obj, 'data')) {
			nms <- names(obj)
			if (length(field) <= length(nms)) {
				m <- match(field, nms)
				if (!all(is.na(m))) {
					m <- stats::na.omit(m)
					return(obj@data[, m, drop=FALSE])
				}
			}
		} 
	}

	if (NROW(field) == n) {
		if (is.null(nrow(field))) {
			return(data.frame(field, stringsAsFactors=FALSE))
		} else {
			return(field)
		}
		
	} 

	if (is.numeric(field)) {
		putvals <- rep(field, length.out=n)
		return(data.frame(field=putvals))
	}
	
	stop('invalid value for field') 
}


.intersectSegments <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
# Translated by RH from LISP code by Paul Reiners
# http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/linesegments.lisp
# Which was translated from the algorithm by Paul Bourke given here: http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
    denom  <-  ((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1))
    ua_num  <- ((x4 - x3) *(y1 - y3)) - ((y4 - y3) * (x1 - x3))
    ub_num  <- ((x2 - x1) *(y1 - y3)) - ((y2 - y1) * (x1 - x3))
# If the denominator and numerator for the equations for ua and ub are 0 then the two lines are coincident.
    if ( denom == 0 ) {
		if (ua_num == 0 & ub_num == 0) {
			xmin <- max(x1, x3)
			if (xmin==x1) {ymin <- y1} else {ymin <- y3}
			xmax <- min(x2, x4)
			if (xmax==x2) {ymax <- y2} else {ymax <- y4}
		# RH: for coincident line (segments) returning two intersections : start and end
			return(rbind(c(xmin, ymin),
							 c(xmax, ymax)))
		} #else {	
# If the denominator for the equations for ua and ub is 0 then the two lines are parallel.
#			return(c(NA, NA))
#		}
	} else {
		ua <- round(ua_num / denom, 12)
		ub <- round(ub_num / denom, 12)
		if ((ua >= 0 & ua <= 1) & (ub >= 0 & ub <= 1) ) {
			x <- x1 + ua * (x2 - x1)
			y <- y1 + ua * (y2 - y1) 
			return(c(x, y))
		}
	} 
	
	return(c(NA, NA))
}


.intersectLinePolygon <- function(line, poly) {
	resxy <- matrix(NA, ncol=2, nrow=0)
	miny <- min(line[,2])
	maxy <- max(line[,2])
	xyxy <- cbind(poly, rbind(poly[-1,], poly[1,]))
    xyxy <- subset(xyxy, !( (xyxy[,2] > maxy & xyxy[,4] > maxy ) | (xyxy[,2] < miny & xyxy[,4] < miny)) )
	if (nrow(xyxy) == 0) { 
		return(resxy) 
	}
	for (i in 1:nrow(xyxy)) {
		xy <- .intersectSegments(xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4], line[1,1], line[1,2], line[2,1], line[2,2] )
		if (!is.na(xy[1])) {
			resxy <- rbind(resxy, xy)
		}
	}
	return((resxy))
}





.polygonsToRaster <- function(p, rstr, field, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue="all", getCover=FALSE, filename="", silent=TRUE, faster=TRUE, ...) {


	npol <- length(p@polygons)
	pvals <- .getPutVals(p, field, npol, mask)
	putvals <- pvals[,1]
	if (ncol(pvals) > 1) {
		rstr@data@isfactor <- TRUE
		rstr@data@attributes <- list(pvals)
		if (!is.character(fun)) {
			stop('when rasterizing multiple fields you must use "fun=first" or "fun=last"')
		} else if (!(fun %in% c('first', 'last'))) {
			stop('when rasterizing multiple fields you must use "fun=first" or "fun=last"')
		}
	}


	if (getCover) {
		nc <- ncell(rstr)
		# high precision for possibly small polygons
		#https://stackoverflow.com/questions/53854910/issue-with-estimating-weighted-mean-from-raster-for-a-polygon-shape-in-r/
		fctr <- ifelse(nc < 5, 100, ifelse(nc < 17, 20, 10))
		rstr <- disaggregate(raster(rstr), fctr)
		r <- .fasterize(p, rstr, rep(1, npol), background=0, datatype="INT1U") 
		return( aggregate(r, fctr, mean, na.rm=TRUE, filename=filename, ...) )
	} 
	

	
	### new code
	if (is.character(fun) && (ncol(pvals) == 1) && faster) {

		if (fun == "last") {
			if (mask || update) {
				if (mask && update) stop("either use 'mask' OR 'update'")	
				background = NA
				r <- .fasterize(p, rstr, pvals[,1], background) 
				if (! hasValues(r)) {
					if (mask) { 
						warning('there are no values to mask')
					} else {
						warning('there are no values to update')
					}
					return(r)
				}
				if (mask) {
					r <- mask(rstr, r)
				} else {
					if (updateValue[1]=="all") {
						r <- cover(r, rstr)
					} else if (updateValue[1]=="NA") {
						r <- cover(rstr, r, ...)
					} else if (updateValue[1]=="!NA") {
						r <- mask(cover(r, rstr), rstr, ...)
					} else {
						s <- stack(r, rstr)
						r <- overlay(rstr, r, fun=function(x,y){ i = (x %in% updateValue & !is.na(y)); x[i] <- y[i]; x }, ... )
					}
				}
				return(r)
			} else {
				return( .fasterize(p, rstr, pvals[,1], background, filename, ...) )
			}
		}
		
	}
	### end new code


	
	leftColFromX <- function ( object, x )	{
		colnr <- (x - xmin(object)) / xres(object)
		i <- colnr %% 1 == 0
		colnr[!i] <- trunc(colnr[!i]) + 1 
		colnr[colnr <= 0] <- 1
		colnr
	}


	rightColFromX <- function ( object, x )	{
		colnr <- trunc((x - xmin(object)) / xres(object)) + 1 
		colnr[ colnr > ncol(object) ] <- object@ncols
		colnr
	}

		
	if (! inherits(p, 'SpatialPolygons') ) {
		stop('The first argument should be an object of the "SpatialPolygons*" lineage')
	}
						
	filename <- trim(filename)
	if (!canProcessInMemory(rstr, 3) && filename == '') {
		filename <- rasterTmpFile()
	}
	


	if (mask & update) { 
		stop('use either "mask" OR "update"')
	} else if (mask) { 
		oldraster <- rstr 
		#update <- TRUE 
	} else if (update) {
		oldraster <- rstr 
		if (!is.numeric(updateValue)) {
			if (is.na(updateValue)) {
				updateValue <- 'NA'
			} else if (!(updateValue == 'NA' | updateValue == '!NA' | updateValue == 'all')) {
				stop('updateValue should be either "all", "NA", "!NA"')
			}
		} 
	}
	
	rstr <- raster(rstr)
	
	if (!is.na(projection(p))) {
		projection(rstr) <- .getSRS(p)
	}

# check if bbox of raster and p overlap
	spbb <- sp::bbox(p)
	rsbb <- bbox(rstr)
	if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
		# instead of a warning
		return( init(rstr, function(x) NA) )
		# so that clusterR can use this function (overlap with some chunks might be NULL)
	}
	
	npol <- length(p@polygons)
	pvals <- .getPutVals(p, field, npol, mask)
	putvals <- pvals[,1]
	if (ncol(pvals) > 1) {
		rstr@data@isfactor <- TRUE
		rstr@data@attributes <- list(pvals)
		if (!is.character(fun)) {
			stop('when rasterizing multiple values you must use "fun=first" or "fun=last"')
		} else if (!(fun %in% c('first', 'last'))) {
			stop('when rasterizing multiple values you must use "fun=first" or "fun=last"')
		}
	}

	if (is.character(fun)) {
		if (fun=='first') {
			fun <- function(x, ...){ stats::na.omit(x)[1] } 
		} else if (fun=='last') {
			fun <- function(x, ...){ rev(stats::na.omit(x))[1] }
		} else if (fun == 'count') {
			fun <- function(x, ...){ sum(!is.na(x)) }
			field <- 1
		}
	}
	
	polinfo <- data.frame(matrix(NA, nrow=npol * 2, ncol=6))
	colnames(polinfo) <- c('part', 'miny', 'maxy', 'value', 'hole', 'object')
	addpol <- polinfo[rep(1, 500), ]
	rownames(addpol) <- NULL
	pollist <- list()
	cnt <- 0
	for (i in 1:npol) {
		nsubpol <- length(p@polygons[[i]]@Polygons)
		for (j in 1:nsubpol) {
			cnt <- cnt + 1
			if (cnt > dim(polinfo)[1]) { 
				polinfo <- rbind(polinfo, addpol)  
			}
			polinfo[cnt, 1] <- cnt
			polinfo[cnt, 2] <- min(p@polygons[[i]]@Polygons[[j]]@coords[,2])
			polinfo[cnt, 3] <- max(p@polygons[[i]]@Polygons[[j]]@coords[,2])
			polinfo[cnt, 4] <- putvals[i]
			if ( p@polygons[[i]]@Polygons[[j]]@hole ) {
				polinfo[cnt, 5] <- 1
			} else {
				polinfo[cnt, 5] <- 0
			}
			polinfo[cnt, 6] <- i
			pollist[[cnt]] <- p@polygons[[i]]@Polygons[[j]]
		}
	}
	
	if (! silent) { 
		message('Found ', npol, ' region(s) and ', cnt, ' polygon(s)') 
	}
	
	polinfo <- subset(polinfo, polinfo[,1] <= cnt, drop=FALSE)
#	polinfo <- polinfo[order(polinfo[,1]),]
#	rm(p)
		
	lxmin <- min(spbb[1,1], rsbb[1,1]) - xres(rstr)
	lxmax <- max(spbb[1,2], rsbb[1,2]) + xres(rstr)
	
#	if (getCover) { 
#		return (.polygoncover(rstr, filename, polinfo, lxmin, lxmax, pollist, ...)) 
#	}

	adj <- 0.5 * xres(rstr)

	if (filename == "") {
		v <- matrix(NA, ncol=nrow(rstr), nrow=ncol(rstr))
	} else {
		rstr <- writeStart(rstr, filename=filename, ...)
	}

	rxmn <- xmin(rstr) 
	rxmx <- xmax(rstr) 
	
	rv1 <- rep(NA, ncol(rstr))
	holes1 <- rep(0, ncol(rstr))
	
	pb <- pbCreate(nrow(rstr), label='rasterize', ...)

	for (r in 1:nrow(rstr)) {
		
		vals <- NULL
		holes <- holes1

		ly <- yFromRow(rstr, r)
		myline <- rbind(c(lxmin,ly), c(lxmax,ly))
		
		subpol <- subset(polinfo, !(polinfo[,2] > ly | polinfo[,3] < ly), drop=FALSE)
		if (length(subpol[,1]) > 0) {
			updateHoles <- FALSE
			lastpolnr <- subpol[1,6]
			rvtmp <- rv1
			for (i in 1:nrow(subpol)) {
				if (i == nrow(subpol)) { 
					updateHoles <- TRUE 
				} else if (subpol[i+1,6] > lastpolnr) { # new polygon
					updateHoles <- TRUE 
					lastpolnr <- subpol[i+1,6]
				}
				
				mypoly <- pollist[[subpol[i,1]]]
				intersection <- .intersectLinePolygon(myline, mypoly@coords)
				#if (nrow(intersection) %% 2 == 1) {
				# this is a bit speculative
				# not OK!
				#	intersection <- unique(intersection)
				#}

				x <- sort(intersection[,1])
				if (length(x) > 0) {
					if ((nrow(intersection) %% 2 == 1) || ( sum(x[-length(x)] == x[-1]) > 0 )) {
					# uneven number or duplicates
					# e.g. single node intersection going out of polygon ....
						spPnts <- sp::SpatialPoints(xyFromCell(rstr, cellFromRowCol(rstr, rep(r, ncol(rstr)), 1:ncol(rstr))))
						spPol <- sp::SpatialPolygons(list(sp::Polygons(list(mypoly), 1)))
						over <- sp::over(spPnts, spPol)
						if ( subpol[i, 5] == 1 ) {
							holes[!is.na(over)] <- holes[!is.na(over)] - 1
						} else {
							rvtmp[!is.na(over)] <- subpol[i,4] 
							holes[!is.na(over)] <- holes[!is.na(over)] + 1
						}
						# print(paste('exit node intersection on row:', r))
					} else {
					
						for (k in 1:round(nrow(intersection)/2)) {
							l <- (k * 2) - 1		
							x1 <- x[l]
							x2 <- x[l+1]
							#if (is.na(x2)) { 
							#	txt <- paste('something funny at row:', r, 'polygon:',j)
							#	stop(txt)
							#}
							#  if (x1 > rxmx) { next }
							# if (x2 < rxmn) { next }
							# adjust to skip first cell if the center is not covered by this polygon
							x1a <- x1 + adj
							x2a <- x2 - adj
							if (x1a > rxmx) { next }
							if (x2a < rxmn) { next }
							x1a <- min(rxmx, max(rxmn, x1a))
							x2a <- min(rxmx, max(rxmn, x2a))
							col1 <- leftColFromX(rstr, x1a)
							col2 <- rightColFromX(rstr, x2a)
							if (col1 > col2) { 
								spPnts <- sp::SpatialPoints(xyFromCell(rstr, cellFromRowCol(rstr, rep(r, ncol(rstr)), 1:ncol(rstr))))
								spPol <- sp::SpatialPolygons(list(sp::Polygons(list(mypoly), 1)))
								over <- sp::over(spPnts, spPol)
								if ( subpol[i, 5] == 1 ) {
									holes[!is.na(over)] <- holes[!is.na(over)] - 1
								} else {
									rvtmp[!is.na(over)] <- subpol[i,4] 
									holes[!is.na(over)] <- holes[!is.na(over)] + 1
								}
								next
							}
							if ( subpol[i, 5] == 1 ) {
								holes[col1:col2] <- holes[col1:col2] - 1
							} else {
								rvtmp[col1:col2] <- subpol[i,4]
								holes[col1:col2] <- holes[col1:col2] + 1
							}
						}
					}
				}	
								
				if (updateHoles) {
					updateHoles <- FALSE
					rvtmp[holes < 1] <- NA
					vals <- cbind(vals, rvtmp)
					rvtmp <- rv1
					holes <- holes1
				}	
			}
		}	
		
		#print(vals)
		
		rrv <- rv1
		if (!is.null(vals)) {
			u <- which(rowSums(is.na(vals)) < ncol(vals))
			if (length(u) > 0) {
				if (mask) {
					rrv[u] <- 1
				} else {
					rrv[u] <- apply(vals[u, ,drop=FALSE], 1, fun, na.rm=TRUE)
				} 
			}
		}
		
		if (mask) {
			oldvals <- getValues(oldraster, r)
			ind <- which(is.na(rrv))
			oldvals[ind] <- NA
			rrv <- oldvals
		} else if (update) {
			oldvals <- getValues(oldraster, r)
			if (is.numeric(updateValue)) {
				ind <- which(oldvals == updateValue & !is.na(rrv))
			} else if (updateValue == "all") {
				ind <- which(!is.na(rrv))
			} else if (updateValue == "NA") {
				ind <- which(is.na(oldvals))
			} else { "!NA"
				ind <- which(!is.na(oldvals) & !is.na(rrv))
			}
			oldvals[ind] <- rrv[ind]
			rrv <- oldvals
		} else {
			rrv[is.na(rrv)] <- background
		}

		if (filename == "") {
			v[,r] <- rrv
		} else {
#			print(rrv)
			rstr <- writeValues(rstr, rrv, r)
		}
		pbStep(pb, r)
	}
	pbClose(pb)

	if (filename == "") {
		rstr <- setValues(rstr, as.vector(v))
	} else {
		rstr <- writeStop(rstr)
	}
	return(rstr)
}

#plot( .polygonsToRaster(p, rstr) )


#...polygoncover <- function(p, x, filename, ...) {
#	d <- disaggregate(raster(x), 10)
#	r <- .polygonsToRaster(p, d, filename=filename, field=1, fun='first', background=0, mask=FALSE, update=FALSE, getCover=FALSE, silent=TRUE, ...)
#	aggregate(r, 10, sum)
#} 

.Old_polygoncover <- function(rstr, filename, polinfo, lxmin, lxmax, pollist, ...) {
# percentage cover per grid cell

	polinfo[, 4] <- 1

	bigraster <- raster(rstr)
	rxmn <- xmin(bigraster) 
	rxmx <- xmax(bigraster) 
	f <- 10
	adj <- 0.5 * xres(bigraster)/f
	nc <- ncol(bigraster) * f
	rv1 <- rep(0, nc)
	holes1 <- rep(0, nc)
	prj <- .getSRS(bigraster)
	hr <- 0.5 * yres(bigraster)

	vv <- matrix(ncol=f, nrow=nc)
	
	if (filename == "") {
		v <- matrix(NA, ncol=nrow(bigraster), nrow=ncol(bigraster))
	} else {
		bigraster <- writeStart(bigraster, filename=filename, ...)
	}
	
	pb <- pbCreate(nrow(bigraster), label='rasterize', ...)
	for (rr in 1:nrow(bigraster)) {
		y <- yFromRow(bigraster, rr)
		yn <- y - hr
		yx <- y + hr
		rstr <- raster(xmn=rxmn, xmx=rxmx, ymn=yn, ymx=yx, ncols=nc, nrows=f, crs=prj)
		subpol <- subset(polinfo, !(polinfo[,2] > yx | polinfo[,3] < yn), drop=FALSE)
		for (r in 1:f) {
			rv <- rv1
			ly <- yFromRow(rstr, r)
			myline <- rbind(c(lxmin,ly), c(lxmax,ly))
			holes <- holes1
			if (length(subpol[,1]) > 0) { 		

			updateHoles <- FALSE
				lastpolnr <- subpol[1,6]
				rvtmp <- rv1
				for (i in 1:length(subpol[,1])) {
					if (i == length(subpol[,1])) { 
						updateHoles <- TRUE 
					} else if (subpol[i+1,6] > lastpolnr) {
						updateHoles <- TRUE 
						lastpolnr <- subpol[i+1,6]
					}
					
					mypoly <- pollist[[subpol[i,1]]]
					intersection <- .intersectLinePolygon(myline, mypoly@coords)
					x <- sort(intersection[,1])
					if (length(x) > 0) {
						#if (length(subpol[,1]) > 3 & i ==2) { 		
						#	print('4')						
						#}
						if ( sum(x[-length(x)] == x[-1]) > 0 ) {
					# single node intersection going out of polygon ....
							spPnts <- sp::SpatialPoints(xyFromCell(rstr, cellFromRowCol(rstr, rep(r, ncol(rstr)), 1:ncol(rstr))))
							spPol <- sp::SpatialPolygons(list(sp::Polygons(list(mypoly), 1)))
							over <- sp::over(spPnts, spPol)
							if ( subpol[i, 5] == 1 ) {
								holes[!is.na(over)] <- holes[!is.na(over)] - 1
							} else {
								rvtmp[!is.na(over)] <- subpol[i,4] 
								holes[!is.na(over)] <- holes[!is.na(over)] + 1
							}
						} else {
							for (k in 1:round(nrow(intersection)/2)) {
								l <- (k * 2) - 1		
								x1 <- x[l]
								x2 <- x[l+1]
								if (x1 > rxmx) { next }
								if (x2 < rxmn) { next }
							# adjust to skip first cell if the center is not covered by this polygon
								x1a <- x1 + adj
								x2a <- x2 - adj
								x1a <- min(rxmx, max(rxmn, x1a))
								x2a <- min(rxmx, max(rxmn, x2a))
								col1 <- colFromX(rstr, x1a)
								col2 <- colFromX(rstr, x2a)
								if (col1 > col2) { next }
								if ( subpol[i, 5] == 1 ) {
									holes[col1:col2] <- holes[col1:col2] - 1
								} else {
									rvtmp[col1:col2] <- subpol[i,4]
									holes[col1:col2] <- holes[col1:col2] + 1
								}
							}
						}
						if (updateHoles) {
							holes <- holes < 1
							rvtmp[holes] <- 0
							holes <- holes1
							updateHoles <- FALSE
							rv <- pmax(rv, rvtmp)	
						}
					}
				}
			}
			vv[,r] <- rv
		}
		av <- colSums( matrix( rowSums(vv), nrow=f) )
		
		if (filename == "") {
			v[,rr] <- av
		} else {
			bigraster <- writeValues(bigraster, av, rr)
		}
		pbStep(pb, rr)
	}
	pbClose(pb)

	if (filename == "") {
		bigraster <- setValues(bigraster, as.vector(v))
	} else {
		bigraster <- writeStop(bigraster)
	}
	return(bigraster)
}


#x = .polygoncover(rstr, "", polinfo, lxmin, lxmax, pollist)


.polygonsToRaster2 <- function(p, raster, field=0, filename="", ...) {
#  This is based on sampling by points. Should be slower except when  polygons very detailed and raster  has low resolution
# but it could be optimized further
# currently not used. Perhaps it should be used under certain conditions. 
# this version does not deal with polygon holes 

# check if bbox of raster and p overlap
	filename <- trim(filename)
	raster <- raster(raster)
	
	spbb <- sp::bbox(p)
	rsbb <- bbox(raster)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('polygon and raster have no overlapping areas')
	}

	if (inherits(p, 'SpatialPolygons') || (field == 0)) {
		putvals <- 1:length(p@polygons)
	} else {
		putvals <- as.vector(p@data[,field])
		if (inherits(putvals, 'character')) {
			stop('selected field is charater type')
		}
	}
	
	
	if (filename == "") {
		v <- vector(length=0) # replace this
	} else {
		raster <- writeStart(raster, filename=filename, ...)
	}
	
	rowcol <- cbind(0, 1:ncol(raster))

	firstrow <- rowFromY(raster, spbb[2,2])
	lastrow <- rowFromY(raster, spbb[2,1])
	
	for (r in 1:nrow(raster)) {
		if (r < firstrow | r > lastrow) {
			vals <- rep(NA, times=ncol(raster))
		} else {
			rowcol[,1] <- r
			sppoints <- xyFromCell(raster, cellFromRowCol(raster, rowcol[,1], rowcol[,2]), TRUE)
			over <- sp::over(sppoints, p)
			vals <- putvals[over]
		}
		if (filename == "") {
			v <- c(v, vals)
		} else {
			raster <- writeValues(raster, vals)
		}
	}
	if (filename == "") {
		raster <- setValues(raster, v)
	} else {
		raster <- writeStop(raster)
	}
	return(raster)
}

