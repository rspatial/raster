# Author: Robert J. Hijmans
# Date :  January 2009
# Version 0.9
# Licence GPL v3

.specialRowFromY <- function(object, y) {
	rownr <- 1 + (trunc((ymax(object) - y)/yres(object)))
    rownr[y == ymin(object)] <- nrow(object)
    rownr[y > ymax(object)] <- -1
	rownr[y < ymin(object)] <- nrow(object) + 1
	return(rownr)
}

.specialColFromX <- function(object, x) {
	colnr <- (trunc((x - xmin(object))/xres(object))) + 1
    colnr[x == xmax(object)] <- ncol(object)
    colnr[x < xmin(object)] <- -1 
	colnr[x > xmax(object)] <- ncol(object) + 1
    return(colnr)
}



.getCols <- function(rs, rownr, aline, line1, line2) {
	minx <- xmin(rs)
	maxx <- xmax(rs)
	resxy <- matrix(NA, ncol=2, nrow=0)
	miny <- min(line1[,2], line2[,2])
	maxy <- max(line1[,2], line2[,2])
	xyxy <- cbind(aline[1:(length(aline[,1])-1), ,drop=FALSE], aline[-1, ,drop=FALSE])

    xyxy <- subset(xyxy, !( (xyxy[,2] > maxy & xyxy[,4] > maxy ) | (xyxy[,2] < miny & xyxy[,4] < miny)) )
	if (length(xyxy) < 1) { 
		return(resxy) 
	}
	res <- vector(length=0)
	for (i in 1:length(xyxy[,1])) {	
		rows <- .specialRowFromY(rs, c(xyxy[i,2], xyxy[i,4]) )
		if ((rows[1] > rownr & rows[2] > rownr) | (rows[1] < rownr & rows[2] < rownr)) { 
			next
		}
		cols <- .specialColFromX(rs, c(xyxy[i,1], xyxy[i,3]))
		if ((cols[1] < 1 & cols[2] < 1) | (cols[1] > ncol(rs) & cols[2] > ncol(rs))) { 
			next
		}

		rowcol <- cbind(rows, cols)[order(cols),]
		if (rowcol[1,1] == rowcol[2,1]) {
			# entire line segment in row
			add <- rowcol[1,2]:rowcol[2,2]
			add <- subset(add, add>0 & add<=ncol(rs))
			res <- c(res, add)
		} else {
			if (rowcol[1,1] == rownr  ) {
				# line segment starts in this row
				if (rowcol[2,1] < rownr) {
					xy <- .intersectSegments(line1[1,1], line1[1,2], line1[2,1], line1[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4]  )
				} else {
					xy <- .intersectSegments(line2[1,1], line2[1,2], line2[2,1], line2[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4]  )
				}
				if (is.na(xy[1])) { 
					xy <- xyxy[i,3:4]
				}
				xy <- t(as.matrix(xy))
				outcol = min(.specialColFromX(rs, xy[,1]), ncol(rs))
				if (outcol < 1) next
				cols <- c(max(1, rowcol[1,2]), outcol)
				col1 <- min(cols)
				col2 <- max(cols)
				res <- c(res, col1:col2)
			} else if (rowcol[2,1] == rownr) {
				# line segment ends in this row
				if (rowcol[1,1] < rownr) {
					xy <- .intersectSegments(line1[1,1], line1[1,2], line1[2,1], line1[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4] )
				} else {
					xy <- .intersectSegments(line2[1,1], line2[1,2], line2[2,1], line2[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4] )
				}
				if (is.na(xy[1])) { next }
				xy <- t(as.matrix(xy))
				incol <- max(1, .specialColFromX(rs, xy[,1]))
				if (incol > ncol(rs)) next
				cols <- c(incol, min(ncol(rs), rowcol[2,2]))
				col1 <- min(cols)
				col2 <- max(cols)
				res <- c(res, col1:col2)
			} else {
				# line segment crosses this row
				xy1 <- .intersectSegments(line1[1,1], line1[1,2], line1[2,1], line1[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4]  )
				xy2 <- .intersectSegments(line2[1,1], line2[1,2], line2[2,1], line2[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4]  )
				if (is.na(xy1[1])) { next }
				if (is.na(xy2[1])) { next }
				xy <- rbind(xy1, xy2)
				cols <- .specialColFromX(rs, xy[,1])
				col1 <- min(cols)
				col2 <- max(cols)
				if (col1 > ncol(rs)) { next }
				if (col2 == -1) {  next }
				if (col1 == -1) { col1 <- 1 }
				if (col2 > ncol(rs)) { col2 <- ncol(rs) }
				res <- c(res, col1:col2)
			}
		}
	}
	return(res)
}




.rasterizeLineLength <- function(x, r, background=NA, filename="", ...) {

	on.exit(rgeos::set_RGEOS_CheckValidity(.checkGEOS()))
	r <- raster(r)

	if (canProcessInMemory(r, n=8)) {
		r[] <- 1:ncell(r)
		
		rp <- rasterToPolygons(r)
		rp <- intersect(x, rp)
		lengths <- rgeos::gLength(rp, byid=TRUE) / 1000
		
		n <- tapply(lengths, data.frame(rp)[, names(r)], sum)
		
		out <- setValues(r, background)
		out[as.integer(names(n))] <- n
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		}
		return(out)
		
	} else {
	
		out <- raster(r)
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='rasterize', ...)
		out <- writeStart(out, filename=filename, ...)
		nc <- ncol(out)
		for (i in 1:tr$n) {
			y <- crop(r, extent(r, tr$row[i], tr$row[i] + tr$nrows[i] - 1, 1, nc))
			y[] <- 1:ncell(y)
			rp <- rasterToPolygons(y, na.rm=FALSE)
			rp <- intersect(x, rp)
			lengths <- rgeos::gLength(rp, byid=TRUE) / 1000
			n <- tapply(lengths, data.frame(rp)[, names(y)], sum)
			v <- rep(background, ncell(y))
			v[as.integer(names(n))] <- n 
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb)
		}
		pbClose(pb)
		out <- writeStop(out)
		return(out)
	}
}



.linesToRaster <- function(lns, x, field, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue="all", filename="", ...) {

	dots <- list(...)
	if (!is.null(dots$overlap)) { stop('argument "overlap" is no longer available. Use "fun"') } 
	if (!is.null(dots$updateRaster)) { stop('argument "updateRaster" is no longer available. Use "update"') } 
	
	
	filename <- trim(filename)

	if (mask & update) { 
		stop('use either "mask=TRUE" OR "update=TRUE" (or neither)')
	}
	
	if (update) {
		if (!is.numeric(updateValue)) {
			if (is.na(updateValue)) {
				updateValue <- 'NA'
			} else if (!(updateValue == 'NA' | updateValue == '!NA' | updateValue == 'all')) {
				stop('updateValue should be either "all", "NA", "!NA"')
			}
		} 
	}

	
	if (is.character(fun)) {
		if (!(fun %in% c('first', 'last', 'sum', 'min', 'max', 'count', 'length'))) {
			stop('invalid character value for fun')
		}
		doFun <- FALSE
		if (fun == 'length') {
			if (mask) {
				fun <- 'first'
			} else if (update) {
				stop('cannot do update with length yet --- come back later...')
			} else {
				return(.rasterizeLineLength(lns, x, background=background, update=FALSE, updateValue="all", filename="", ...) )
			}
		}
	} else {
		doFun <- TRUE
	}
	
	rstr <- raster(x)
	if (!is.na(projection(lns))) {
		projection(rstr) <-.getCRS(lns)
	}
	
	if (inherits(lns, 'SpatialPolygons')) {
		lns <- as(lns, "SpatialLines")
	}
	if (! inherits(lns, 'SpatialLines')) {
		stop('lns should be, or inherit from, a SpatialLines* object')
	}

# check if bbox of raster and lns overlap
	spbb <- bbox(lns)
	rsbb <- bbox(rstr)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('lines and raster have no overlapping areas')
	}
	nline <- length(lns@lines)
	info <- matrix(NA, nrow=nline, ncol=4)
	info[,4] <- 1:nrow(info)
	info[,1] <- sapply(lns@lines, function(i) length(i@Lines))
	for (i in 1:nline) {
		r <- range(sapply( lns@lines[[i]]@Lines, function(j) range(j@coords[,2])))
		info[i,2] <- r[1]
		info[i,3] <- r[2]
	}
	
	
	lxmin <- min(spbb[1,1], rsbb[1,1]) - 0.5 * xres(rstr)
	lxmax <- max(spbb[1,2], rsbb[1,2]) + 0.5 * xres(rstr)
	

	pvals <- .getPutVals(lns, field, nline, mask)
	putvals <- pvals[,1]
	if (ncol(pvals) > 1) {
		rstr@data@isfactor <- TRUE
		rstr@data@attributes <- list(pvals)
	}
	
	
	
	if (filename == "") {
		v <- matrix(NA, ncol=nrow(rstr), nrow=ncol(rstr))
	} else {
		rstr <- writeStart(rstr, filename=filename, ...)
	}
	rv1 <- rep(NA, ncol(rstr))
	lst1 <- vector(length=length(rv1), mode='list')

	yrs <- yres(rstr)
	
	pb <- pbCreate(nrow(rstr), label='rasterize', ...)
	for (r in 1:nrow(rstr)) {
		ly <- yFromRow(rstr, r)
		uly <- ly + 0.51 * yrs
		lly <- ly - 0.51 * yrs

		info1 <- subset(info,     !(info[,2] > uly   | info[,3] < lly ) )
#		subpol <- subset(polinfo, !(polinfo[,2] > ly | polinfo[,3] < ly), drop=FALSE)
		if (doFun) { rv <- lst1
		} else { rv <- rv1	}
		
		if (nrow(info1) > 0) { 

			line1 <- rbind(c(lxmin, ly + 0.5*yrs), c(lxmax,ly + 0.5*yrs))
			line2 <- rbind(c(lxmin, ly - 0.5*yrs), c(lxmax,ly - 0.5*yrs))
		


			for (k in 1:nrow(info1)) {
				i <- info1[k,4]
				for (j in 1:info1[k,1]) {
					if ( max ( lns@lines[[i]]@Lines[[j]]@coords[,2] ) < lly  |  min( lns@lines[[i]]@Lines[[j]]@coords[,2] ) > uly ) {
						#  line part entirely outside of row. do nothing
					} else {
						aline <- lns@lines[[i]]@Lines[[j]]@coords
						#cat(i, "\n"); utils::flush.console();
						colnrs <- .getCols(rstr, r, aline, line1, line2)
						if ( length(colnrs) > 0 ) {	
							rvtmp <- rv1
							rvtmp[colnrs] <- putvals[i]
							
							
							if (doFun) {
								ind <- which(!is.na(rvtmp))
								for (ii in ind) {
									rv[[ii]] <- c(rv[[ii]], rvtmp[ii])
								}
							} else if (mask) {
								rv[!is.na(rvtmp)] <- rvtmp[!is.na(rvtmp)]
							} else if (fun=='last') {
								rv[!is.na(rvtmp)] <- rvtmp[!is.na(rvtmp)]
							} else if (fun=='first') {
								rv[is.na(rv)] <- rvtmp[is.na(rv)]
							} else if (fun=='sum') {
								rv[!is.na(rv) & !is.na(rvtmp)] <- rv[!is.na(rv) & !is.na(rvtmp)] + rvtmp[!is.na(rv) & !is.na(rvtmp)] 
								rv[is.na(rv)] <- rvtmp[is.na(rv)]
							} else if (fun=='min') {
								rv[!is.na(rv) & !is.na(rvtmp)] <- pmin(rv[!is.na(rv) & !is.na(rvtmp)], rvtmp[!is.na(rv) & !is.na(rvtmp)])
								rv[is.na(rv)] <- rvtmp[is.na(rv)]
							} else if (fun=='max') {
								rv[!is.na(rv) & !is.na(rvtmp)] <- pmax(rv[!is.na(rv) & !is.na(rvtmp)], rvtmp[!is.na(rv) & !is.na(rvtmp)])
								rv[is.na(rv)] <- rvtmp[is.na(rv)]
							} else if (fun=='count') {
								rvtmp[!is.na(rvtmp)]  <- 1
								rv[!is.na(rv) & !is.na(rvtmp)] <- rv[!is.na(rv) & !is.na(rvtmp)] + rvtmp[!is.na(rv) & !is.na(rvtmp)] 
								rv[is.na(rv)] <- rvtmp[is.na(rv)]				
							}							
						}
					}
				}
			
			}
		}
		
		if (doFun) {
			for (i in 1:length(rv)) {
				if (is.null(rv[[i]])) {
					rv[[i]] <- NA
				}
			}
			rv <- sapply(rv, fun)
		}
		
		if (mask) {
			oldvals <- getValues(x, r)
			ind <- which(is.na(rv))
			oldvals[ind] <- NA
			rv <- oldvals
		} else if (update) {
			oldvals <- getValues(x, r)
			if (is.numeric(updateValue)) {
				ind <- which(oldvals == updateValue & !is.na(rv))
			} else if (updateValue == "all") {
				ind <- which(!is.na(rv))
			} else if (updateValue == "NA") {
				ind <- which(is.na(oldvals))
			} else {
				ind <- which(!is.na(oldvals) & !is.na(rv))
			}
			oldvals[ind] <- rv[ind]
			rv <- oldvals
		} else {
			rv[is.na(rv)] <- background
		}

		
		if (filename == "") {
			v[,r] <- rv
		} else {
			rstr <- writeValues(rstr, rv, r)
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

