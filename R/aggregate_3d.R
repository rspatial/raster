# Author: Robert J. Hijmans
# Date : July 2010
# Version 1.0
# Licence GPL v3

# October 2012: Major overhaul (including C interface)
# November 2012: fixed bug with expand=F
# June 2014: support for aggregation over z (layers) in addition to x and y


setMethod('aggregate', signature(x='Raster'), 
function(x, fact=2, fun='mean', expand=TRUE, na.rm=TRUE, filename="", ...)  {


	fact <- round(fact)
	lf <- length(fact)
	if (lf == 1) {
		fact <- c(fact, fact, 1)
	} else if (lf == 2) {
		fact <- c(fact, 1)
	} else if (lf > 3) {
		stop('fact should have length 1, 2, or 3')
	}
	if (any(fact < 1)) {
		stop('fact should be > 0')
	}
	if (! any(fact > 1)) {
		warning('all fact(s) were 1, nothing to aggregate')
		return(x)
	}
	xfact <- fact[1]
	yfact <- fact[2]
	zfact <- fact[3]
	
	ncx <- ncol(x)
	nrx <- nrow(x)
	nlx <- nlayers(x)
	if (xfact > ncx) {
		warning('aggregation factor is larger than the number of columns') 
		 xfact <- ncx 
		 if (!na.rm) xfact <- xfact + 1
	}
	if (yfact > nrx) {
		warning('aggregation factor is larger than the number of rows')
		yfact <- nrx
		if (!na.rm) yfact <- yfact + 1
	}
	if (zfact > nlx) {
		warning('aggregation factor is larger than the number of layers')
		zfact <- nlx
		if (!na.rm) zfact <- zfact + 1
	}

	if (expand) {
		rsteps <- as.integer(ceiling(nrx/yfact))
		csteps <- as.integer(ceiling(ncx/xfact))
		lsteps <- as.integer(ceiling(nlx/zfact))
		lastcol <- ncx
		lastrow <- nrx
		lastlyr <- lsteps * zfact
		lyrs <- 1:nlx

	} else 	{
		rsteps <- as.integer(floor(nrx/yfact))
		csteps <- as.integer(floor(ncx/xfact))
		lsteps <- as.integer(floor(nlx/zfact))
		lastcol <- min(csteps * xfact, ncx)
		lastrow <- min(rsteps * yfact, nrx)
		lastlyr <- min(lsteps * zfact, nlx)
		lyrs <- 1:lastlyr
	}

	ymn <- ymax(x) - rsteps * yfact * yres(x)
	xmx <- xmin(x) + csteps * xfact * xres(x)
		
	if (lsteps > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)		
	}
	extent(out) <- extent(xmin(x), xmx, ymn, ymax(x))
	dim(out) <- c(rsteps, csteps, lsteps) 
	ncout <- ncol(out)
	nlout <- nlayers(out)
	if (zfact == 1) {
		names(out) <- names(x)
	}
	
	
	if (! hasValues(x) ) {	
		return(out) 
	}	

	fun <- .makeTextFun(fun)
	if (class(fun) == 'character') { 
		op <- as.integer(match(fun, c('sum', 'mean', 'min', 'max')) - 1)
	} else {
		op <- NA
	}

	# note that it is yfact, xfact, zfact
	dims <- as.integer(c(lastrow, lastcol, length(lyrs), yfact, xfact, zfact))
	
	if (is.na(op)) {

		if ( canProcessInMemory(x)) {
			v <- getValuesBlock(x, 1, lastrow, 1, lastcol, lyrs, format='m')
			v <- .Call("_raster_aggregate_get", v, as.integer(dims), PACKAGE='raster')
			v <- apply(v, 1, fun, na.rm=na.rm)
			out <- setValues(out, v)
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
			return(out)
		} else {

			xx <- brick(x, values=FALSE)
			if (!expand) {
				nrow(xx) <- (nrow(x) %/% yfact) * yfact
			}		
			tr <- blockSize(xx, n=nlayers(x)*xfact*yfact, minrows=yfact)
			st <- round(tr$nrows[1] / yfact) * yfact
			tr$n <- ceiling(lastrow / st)
			tr$row <- c(1, cumsum(rep(st, tr$n-1))+1)
			tr$nrows <- rep(st, tr$n)
			tr$write <- cumsum(c(1, ceiling(tr$nrows[1:(tr$n-1)]/yfact)))
			tr$nrows[tr$n] <-  nrow(xx) - tr$row[tr$n] + 1
			#tr$outrows <- ceiling(tr$nrows/yfact)
			
			pb <- pbCreate(tr$n, label='aggregate', ...)
			x <- readStart(x, ...)	

			out <- writeStart(out, filename=filename, ...)
			for (i in 1:tr$n) {
				dims[1] <- as.integer(tr$nrows[i])
				vals <- getValuesBlock(x, tr$row[i], tr$nrows[i], 1, lastcol, lyrs, format='m')
				vals <- .Call("_raster_aggregate_get", vals, as.integer(dims), PACKAGE='raster')
				vals <- apply(vals, 1, fun, na.rm=na.rm)
				out <- writeValues(out, matrix(vals, ncol=nlout), tr$write[i])
				pbStep(pb, i) 
			}
			pbClose(pb)
			out <- writeStop(out)
			x <- readStop(x)

			return(out)	
		}
	}
	# else if (!is.na(op)) {
	
	if ( canProcessInMemory(x)) {
		
		x <- getValuesBlock(x, 1, lastrow, 1, lastcol, format='m')
		out <- setValues(out, .Call("_raster_aggregate_fun", x, dims, as.integer(na.rm), op, PACKAGE='raster'))
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		}
		return(out)
			
	} else {
		
		xx <- brick(x, values=FALSE)
		if (!expand) {
			nrow(xx) <- (nrow(x) %/% yfact) * yfact
		}		
		tr <- blockSize(xx, minrows=yfact)
		st <- round(tr$nrows[1] / yfact) * yfact
		tr$n <- ceiling(lastrow / st)
		tr$row <- c(1, cumsum(rep(st, tr$n-1))+1)
		tr$nrows <- rep(st, tr$n)
		tr$write <- cumsum(c(1, ceiling(tr$nrows[1:(tr$n-1)]/yfact)))
		tr$nrows[tr$n] <-  nrow(xx) - tr$row[tr$n] + 1
		#tr$outrows <- ceiling(tr$nrows/yfact)
		
		pb <- pbCreate(tr$n, label='aggregate', ...)
		x <- readStart(x, ...)	

		out <- writeStart(out, filename=filename, ...)

		for (i in 1:tr$n) {
			dims[1] = tr$nrows[i]
			vals <- getValuesBlock(x, tr$row[i], tr$nrows[i], 1, lastcol, format='m')
			vals <- .Call("_raster_aggregate_fun", vals, dims, na.rm, op, PACKAGE='raster')
			out <- writeValues(out, vals, tr$write[i])
			pbStep(pb, i) 
		}

		pbClose(pb)
		out <- writeStop(out)
		x <- readStop(x)

		return(out)	
	}
}
)

