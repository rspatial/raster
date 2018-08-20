# Author: Robert J. Hijmans
# Date:  March 2011
# Version 1
# Licence GPL v3

	
if (!isGeneric("stackSelect")) {
	setGeneric("stackSelect", function(x, y, ...)
		standardGeneric("stackSelect"))
}	

setMethod('stackSelect', signature(x='RasterStackBrick', y='Raster'), 
function(x, y, recycle=FALSE, type='index', filename='', ...) {

	filename <- trim(filename)
	out <- brick(x, values=FALSE)	
	nlx <- nlayers(out)
	nly <- nlayers(y)
	compareRaster(out, y)

	if (recycle) {
		stopifnot(nly > 1)
		stopifnot(nlx > nly)
		stopifnot(nlx %% nly == 0)
		type <- tolower(type)
		stopifnot(type %in% c('index', 'truefalse'))
		nl <- nlx+nlx+nly
		maxnl <- nly
		nr <- nlx / nly
		id <- as.integer( (rep(1:nr, each=nly)-1) * nly )
		
	} else {
		if (nly == 1) {
			out <- raster(out)
		} else {
			out@data@nlayers <- nlayers(y)
		}
		nl <- nlx+nly
		maxnl <- nlx
		id <- 0
	}

	ib <- (nlx+1):(nlx+nly)

	if (canProcessInMemory(x, nl)) {
	
		y <- round(getValues(y))
		if (type == 'truefalse') {
			y <- t(apply(y,1,function(x)x*(1:nly)))
		}
		y[y < 1 | y > maxnl] <- NA
		x <- cbind(getValues(x), y)
		x <- apply(x, 1, function(z) z[z[ib]+id] )
		out <- setValues(out, t(x))
		if (filename != "") {
			out <- writeRaster(out, filename=filename, ...)
		}
		
	} else {
	
		if (filename == '') { filename <- rasterTmpFile() }
	
		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out, n=nlx+nly)
		pb <- pbCreate(tr$n, ...)

		for (i in 1:tr$n) {
			j <- round(getValues(y, row=tr$row[i], nrows=tr$nrows[i]))
			if (type == 'truefalse') {
				j <- t(apply(j, 1, function(x)x*(1:nly)))
			}
			j[j < 1 | j > maxnl] <- NA
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			v <- cbind(v, j)
			v <- apply(v, 1, function(z) z[z[ib]+id] )
			out <- writeValues(out, t(v), tr$row[i])
			pbStep(pb) 
		}
		out <- writeStop(out)
		pbClose(pb)
	}
	return(out)
} )
		
