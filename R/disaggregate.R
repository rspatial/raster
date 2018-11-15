# Author: Robert Hijmans
# Date : October 2008 - December 2011
# Version 1.0
# Licence GPL v3

# April 2012: Several patches & improvements by Jim Regetz




setMethod('disaggregate', signature(x='Raster'), 
function(x, fact=NULL, method='', filename='', ...) {

	method <- tolower(method)
	if (!method %in% c('bilinear', '')) {
		stop('unknown "method". Should be "bilinear" or ""')
	}
	
	stopifnot(!is.null(fact))
	fact <- as.integer(round(fact))
	if (length(fact)==1) {
		if (fact == 1) 	return(x) 
		if (fact < 2) { stop('fact should be >= 1') }
		xfact <- yfact <- fact
	} else if (length(fact)==2) {
		xfact <- fact[1]
		yfact <- fact[2]
		if (xfact < 1) { stop('fact[1] should be > 0') } 
		if (yfact < 1) { stop('fact[2] should be > 0') }
		if (xfact == 1 & yfact == 1) { return(x) }
	} else {
		stop('length(fact) should be 1 or 2')
	}

	filename <- trim(filename)

	nl <- nlayers(x)
	if (nl > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)
	}

	ncx <- ncol(x)
	nrx <- nrow(x)
	dim(out) <- c(nrx * yfact, ncx * xfact) 
	names(out) <- names(x)
	
	if (! inherits(x, 'RasterStack')) {
		if (! inMemory(x)  & ! fromDisk(x) ) {
			return(out)
		}
	}
	
	if (method=='bilinear') {
		return(resample(x, out, method='bilinear', filename=filename, ...))
	} 
	
		
	
	if (canProcessInMemory(out, 3)) { 

		x <- getValues(x)
		cols <- rep(seq.int(ncx), each=xfact)
		rows <- rep(seq.int(nrx), each=yfact)
		cells <- as.vector( outer(cols, ncx*(rows-1), FUN="+") )
		if (nl > 1) {
			x <- x[cells, ]			
		} else {
			x <- x[cells]			
		}
		out <- setValues(out, x)
		if (filename != '') {
			out <- writeRaster(out, filename=filename,...)
		}
		
	} else { 
	
		tr <- blockSize(x, n=nlayers(x) * prod(fact))
		rown <- (tr$row-1) * yfact + 1
		pb <- pbCreate(tr$n, label='disaggregate', ...)
		if (is.null(list(...)$datatype)) {
			out <- writeStart(out, filename=filename, datatype=.commonDataType(dataType(x)), ...)
		} else {		
			out <- writeStart(out, filename=filename, ...)
		}
		x <- readStart(x, ...)		
		
		cols <- rep(seq.int(ncx), each=xfact)
		rows <- rep(seq.int(tr$nrows[1]), each=yfact)
		cells <- as.vector( outer(cols, ncx*(rows-1), FUN="+") )

		for (i in 1:tr$n) {
			if (i == tr$n) {
				if (tr$nrows[i] != tr$nrows[1]) {
					rows <- rep(seq.int(tr$nrows[i]), each=yfact)
					cells <- outer(cols, ncx*(rows-1), FUN="+")
				}
			}
			v <- getValues(x, tr$row[i], tr$nrows[i])
			if (nl > 1) {
				v <- v[cells, ]
			} else {
				v <- v[cells]			
			}
			out <- writeValues(out, v, rown[i])
			pbStep(pb, i)
		}
	
		out <- writeStop(out)
		x <- readStop(x)
		pbClose(pb)			
	
	}
	return(out)
}
)

