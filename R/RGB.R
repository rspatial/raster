# Author: Robert J. Hijmans
# Date :  February 2014
# Version 1.0
# Licence GPL v3

# partly based on functions in the pixmap package by Friedrich Leisch

if (!isGeneric("RGB")) {
	setGeneric("RGB", function(x, ...)
		standardGeneric("RGB"))
}	




setMethod("RGB", signature(x='RasterLayer'), 
function(x, filename='', col=rainbow(25),  breaks=NULL, alpha=FALSE, colNA='white',zlim=NULL, zlimcol=NULL, ext=NULL,  ...) { 

	getCols <- function(x, col, breaks=NULL, r=NULL, colNA=NA) {
		if (!is.null(breaks)) {
			breaks <- sort(breaks)
			x <- as.numeric(cut(x, breaks, include.lowest=TRUE))
			
		} else {
			x <- (x - r[1])/ (r[2] - r[1])
			x <- round(x * (length(col)-1) + 1)
		}
		x <- col[x]
		if (!is.na(colNA)) {
			x[is.na(x)] <- grDevices::rgb(t(grDevices::col2rgb(colNA)), maxColorValue=255)
		}
		x
	}

	if (!is.null(ext)) {
		x <- crop(x, ext)
	}
	
	if (alpha) {
		out <- brick(x, nl=4, values=FALSE)
	} else {
		out <- brick(x, nl=3, values=FALSE)
	}
	names(out) <- c('red', 'green', 'blue', 'alpha')[1:nlayers(out)]

	if (canProcessInMemory(out)) {

		x <- getValues(x)
		if (is.logical(x)) {
			x <- as.integer(x)
		}

		x[is.infinite(x)] <- NA
		if (!is.null(zlim)) {
			if (!is.null(zlimcol)) {
				x[x < zlim[1]] <- zlim[1]
				x[x > zlim[2]] <- zlim[2]
			} else { #if (is.na(zlimcol)) {
				x[x < zlim[1] | x > zlim[2]] <- NA
			} 
		}
		
		w <- getOption('warn')
		options('warn'=-1) 
		if (is.null(breaks)) {
			zrange <- range(x, zlim, na.rm=TRUE)
		} else {
			zrange <- range(x, zlim, breaks, na.rm=TRUE)
		}
		options('warn'=w) 

		if (zrange[1] == zrange[2]) {
			zrange[1] <- zrange[1] - 0.001
			zrange[2] <- zrange[2] + 0.001
		}

		
		x <- getCols(x, col, breaks, zrange, colNA)
		x <- grDevices::col2rgb(x, alpha=alpha)
		out <- setValues(out, t(x))
		
		if (filename != '') {
			out <- writeRaster(out, filename, datatype='INT2U', ...)
		} 
		return(out)
	} else {
		
		r <- c(minValue(x), maxValue(x))
		if (is.null(breaks)) {
			zrange <- range(r, zlim, na.rm=TRUE)
		} else {
			zrange <- range(r, zlim, breaks, na.rm=TRUE)
		}
		if (zrange[1] == zrange[2]) {
			zrange[1] <- zrange[1] - 0.001
			zrange[2] <- zrange[2] + 0.001
		}

		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='RGB', ...)
		out <- writeStart(out, filename=filename, ...)
		
		for (i in 1:tr$n) {
			v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
			
			if (!is.null(zlim)) {
				if (!is.null(zlimcol)) {
					v[v < zlim[1]] <- zlim[1]
					v[v > zlim[2]] <- zlim[2]
				} else { #if (is.na(zlimcol)) {
					v[v < zlim[1] | v > zlim[2]] <- NA
				} 
			}
			v <- getCols(v, col, breaks, zrange, colNA)
			v <- grDevices::col2rgb(as.vector(v), alpha=alpha)
			out <- writeValues(out, t(v), tr$row[i])
			pbStep(pb)
		}
		pbClose(pb)
		return ( writeStop(out) )
	}
}
)

#x = raster(nr=10, nc=10)
#x[] = 1:100
#y = RGB(x)
#plotRGB(y)



