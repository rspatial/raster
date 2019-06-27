# Author: Robert J. Hijmans
# Date : February 2011
# Version 1.0
# Licence GPL v3


terrain <- function(x, opt='slope', unit='radians', neighbors=8, filename='', ...) {
	
	if (nlayers(x) > 1) {
		warning('first layer of x is used')
		x <- subset(x, 1)
	}
	stopifnot(hasValues(x))
	
	stopifnot(is.character(filename))
	filename <- trim(filename)

	stopifnot(is.character(opt))
	opt <- unique(trim(tolower(opt)))
	i <- which(! opt %in% c('tri', 'tpi', 'roughness','slope', 'aspect', 'flowdir'))
	if (length(i) > 0) {
		stop('invalid value in "opt", choose from:\n "tri", "tpi", "roughness", "slope", "aspect", "flowdir"')
	}
	stopifnot(length(opt) > 0 ) 
	

	nopt <- rep(0, 8)
	if ('tri' %in% opt) {
		nopt[1] <- 1
	} 
	if ('tpi' %in% opt) {
		nopt[2] <- 1
	} 
	if ('roughness' %in% opt) {
		nopt[3] <- 1
	}
	if ('slope' %in% opt) {
		if (neighbors == 4) {
			nopt[4] <- 1
		} else {
			nopt[6] <- 1
		}
	}
	if ('aspect' %in% opt) {
		if (neighbors == 4) {
			nopt[5] <- 1
		} else {
			nopt[7] <- 1
		}
	} 
	if ('flowdir' %in% opt) {
		nopt[8] <- 1
	}
	
	nopt <- as.integer(nopt)
	nl <- sum(nopt)
	
	if (nl == 1) {
		out <- raster(x)
	} else {
		out <- brick(x, values=FALSE, nl=nl)
	}
	names(out) <- c('tri', 'tpi', 'roughness','slope', 'aspect', 'slope', 'aspect', 'flowdir')[as.logical(nopt)]

	rs <- as.double(res(out))
	un <- as.integer(1)
	lonlat <- FALSE
	if ('slope' %in% opt | 'aspect' %in% opt | 'flowdir' %in% opt) {
		stopifnot(is.character(unit))
		unit <- trim(tolower(unit))
		stopifnot(unit %in% c('degrees', 'radians', 'tangent'))
		if (unit=='degrees') {
			un <- as.integer(0)
		} else if (unit=='tangent') {
			un <- as.integer(2)		
		}
		stopifnot(neighbors %in% c(4, 8))
		stopifnot(! is.na(projection(x)) )
		lonlat <- isLonLat(out)
		if (!lonlat & couldBeLonLat(out)) {
			warning('assuming CRS is longitude/latitude')
			lonlat <- TRUE
		}

		if (lonlat) {		
			rs[2] <- pointDistance(cbind(0,0), cbind(0, rs[2]), longlat=TRUE)
		}
	}
	lonlat <- as.integer(lonlat)
	
	
	if (canProcessInMemory(out)) {
		if (lonlat) {
			y <- yFromRow(x, 1:nrow(x))
		} else {
			y <- 0
		}
		v <- .terrain(as.double(values(x)), as.integer(dim(out)), rs, un, nopt, lonlat, y)
		out <- setValues(out, v)
		if (filename  != '') {
			out <- writeRaster(out, filename, ...)
		}
		
	} else {

		out <- writeStart(out, filename, ...)
		tr <- blockSize(out, minblocks=3, minrows=3)
		pb <- pbCreate(tr$n, label='terrain', ...)
		
		nc <- ncol(out)
		buf <- 1:nc
		v <- getValues(x, row=1, nrows=tr$nrows[1]+1)
		y <- 0
		if (lonlat) { y <- yFromRow(out, 1:(tr$nrows[1]+1)) }
		v <- .terrain(as.double(v), as.integer(c(tr$nrows[1]+1, nc)), rs, un, nopt, lonlat, y)
		out <- writeValues(out, matrix(v, ncol=nl), 1)
		pbStep(pb, 1)
		for (i in 2:(tr$n-1)) {
			v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+2)
			if (lonlat) { y <- yFromRow(out, (tr$row[i]-1) : (tr$row[i]+tr$nrows[i])) }
			v <- .terrain(as.double(v), as.integer(c(tr$nrows[i]+2, nc)), rs, un, nopt, lonlat, y)
			v <- matrix(v, ncol=nl)[-buf,]
			out <- writeValues(out, v, tr$row[i])
			pbStep(pb, i)		
		}
		i <- tr$n
		v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+1)
		if (lonlat) { y <- yFromRow(out, (tr$row[i]-1) : (tr$row[i]+tr$nrows[i]-1)) }
		v <- .terrain(as.double(v), as.integer(c(tr$nrows[i]+1, nc)), rs, un, nopt, lonlat, y)
		v <- matrix(v, ncol=nl)[-buf,]
		out <- writeValues(out, v, tr$row[i])
		pbStep(pb, tr$n)
		
		out <- writeStop(out)
		pbClose(pb)
	}
	return(out)
}


# x <- terrain(utm, out='tri')

 